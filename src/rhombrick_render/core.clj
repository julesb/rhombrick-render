(ns rhombrick-render.core
  (:use [quil.core]
        [rhombrick.tilecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.offline]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.bezierbox]
        [clojure.java.io :only [as-file]]
        )
  )

(def ^:dynamic in-states)
(def state-idx (atom 0))
(def num-states (atom 0))
(def camera-pos2 (atom [0.0 0.0 0.0]))
(def mod-scale 1.0)

(def ^:dynamic console-font)
(def particle-texture2 (atom nil))


(defn setup []
  (reset! state-idx 0)
  (reset! bezier-box-control-bias 0.53)
  (reset! bezier-box-resolution 32)
  ;(def console-font (load-font "data/AmericanTypewriter-24.vlw"))
  ;(def console-font (load-font "data/ScalaSans-Caps-32.vlw"))
  ;(def console-font (load-font "UniversLTStd-Light-48.vlw"))
  (def console-font (load-font "Courier-Bold-64.vlw"))
  (smooth)
  (reset! current-topology (topologies :rhombic-dodecahedron))
  (reset! particle-texture2 (load-image "pump_flare_06.png"))
  (texture-mode :normal)
  ;(def in-states (load-all-tiler-states "hex-13Cc-selfcompatible-balanced.state"))
  )


(defn position-camera-ortho [ts]
  (let [[mn1 mx1] (get-assemblage-extents ts)
        mg 1.5; margin
        mn (vec3-sub mn1 [mg mg mg])
        mx (vec3-add mx1 [mg mg mg])
        w  (vec3-distance [(mn 0) 0 0] [(mx 0) 0 0])
        h  (vec3-distance [0 (mn 1) 0] [0 (mx 1) 0])
        d (+ 0 (* (max w h) 0.5))
        c (get-bounding-box-center ts)
        ofx (- (/ (width) 2) (c 0))
        ofy (- (/ (height) 2) (c 1))]
    (ortho (- d) d
           (- d) d
           (mn 2) (mx 2))
    (translate ofx ofy 0)))


(defn position-camera [ts]
  (let [fov (/ Math/PI 4.0)
        rad (+ 0.5 (get-assemblage-radius ts))
        max-rad (get-in ts [:params :max-radius])
        c (* (/ (- max-rad rad) max-rad) 20.0)
        ;c (* rad 2.0)
        ;d (- 20.0 c)
        d (* max-rad 2.0 (Math/sqrt 2.0))
        ]
    (println "RAD:" rad "d:" d "c:" c)
    ;(reset! camera-pos2 [0.0 (* d (Math/sqrt 2.0)) 0.0])
    (reset! camera-pos2 [0.0 d 0.0])
    (perspective fov 1.0 0.1 500.0)
    (camera 0 d 0
            0 0 0
            0 0 -1)))


(defn setup-lights [ts]
  ;(let [max-rad (+ 1 (get-assemblage-radius ts))
  (let [max-rad (get-in ts [:params :max-radius])
        r (* max-rad 4.0)
        mr (/ (- max-rad) 2)]
        ;mr (/ max-rad 2)]
    (lights)
    ;(ambient-light 32 32 32)
    ;(light-falloff 1.1 0.0 0.0)
    (spot-light 192 255 192 mr  r (- r)   1  -1  -1 (/ Math/PI 2) 2)
    (spot-light 255 192 192  r mr (- r)  -1   1  -1 (/ Math/PI 2) 2)
    (spot-light 192 192 255  r  r (- mr)  -1  -1   1 (/ Math/PI 2) 2)))



(defn draw-billboard2 [pos tex]
  (let [[dx dy dz] (vec3-normalize (vec3-sub (vec3-scale pos mod-scale)
                                             @camera-pos2))
      az (Math/atan2 dy dx)
      el (- (Math/asin dz))]
    (rotate az 0 0 1)
    (rotate el 0 1 0)
    (rotate (/ PI 2.0) 0 0 1)
    (begin-shape :quads)
      (texture tex)
      (vertex -1 0 -1 0 0)
      (vertex  1 0 -1 1 0)
      (vertex  1 0  1 1 1)
      (vertex -1 0  1 0 1)
    (end-shape)))


(defn draw-empty2 [ts]
  (no-stroke)
  (blend-mode :screen)
;  (hint :disable-depth-test)
  (tint 128 160 192)
  (doseq [pos (sort-by #(- (vec3-distance % @camera-pos2)) (ts :empty))]
    (with-translation pos
      (scale 2.0)
      (draw-billboard2 pos @particle-texture2)
      ;(draw-faces (@current-topology :verts) (@current-topology :faces) [255 255 0 128])
      ;(box 0.5 0.5 0.5)
      ))
  (no-tint)
  (blend-mode :blend)
  )


(defn draw-tiling3 [ts attr]
  (init-tileset-colors-numcons (get-in ts [:params :tileset]) 0.0)
  ;(init-tileset-colors (get-in ts [:params :tileset]) 0.0)
  (doseq [[pos code] (ts :tiles) ] 
    (let [col (conj (get-tile-color code) 255)
          ;line-col [(col 0) (col 1) (col 2) 160]
          ;line-col col
          line-col [0 0 0 128]
          ;line-col  [192 240 255 240]
          bezier-steps (attr :bbox-res)]
      (with-translation pos
        ;(scale 0.5)
        ;(stroke-weight 8)
        ;(stroke 0 0 0 64)
        (no-stroke)
        (when (attr :simple-lines?)
          (no-fill)
          (draw-facecode-lines code))
        (when (attr :bbox-faces?)
          (if (attr :bbox-smooth?)
            (draw-facecode-bezier-boxes-n ((ts :tiles) pos) col bezier-steps)
            (draw-facecode-bezier-boxes ((ts :tiles) pos) col bezier-steps)))
        (when (attr :bbox-lines?)
          (stroke-weight (attr :bbox-line-weight))
          (no-fill)
          (draw-facecode-bezier-box-lines ((ts :tiles) pos) line-col bezier-steps))
        )
      (when (not= (attr :boundary-mode) :none)
        (draw-face-boundaries-ts ts pos code (attr :boundary-mode)))

      )))


(defn radius-to-scale [r]
  (let [tiling-max-rad 8.0
        rnorm (/ r tiling-max-rad)]
    (+ 53.0 
       (* (- 1.0 rnorm)
          100.0))))
        

(defn radius-to-scale2 [r]
  (let [tiling-max-rad 8.0
        rnorm (/ r tiling-max-rad)]
    (+ 10.0 
       (* (- 1.0 rnorm)
          100.0))))
  

(defn draw-info []
  (text-font console-font)
  (let [ts (in-states @state-idx)
        tileset (get-in ts [:params :tileset])
        tileset-str (str tileset)
        tw (text-width tileset-str)
        x (- (/ (width) 2) (/ tw 2))
        y (- (height) 16)]
        (fill 0 0 0 255)
        (text tileset-str x y)
    )
  )

(defn get-bg-color [tileset]
  ;(let [tileset-num (/ (tileset-to-binary-number tileset) 4096.0)
  (let [tileset-num (tileset-to-binary-number tileset)
        col (vec3-add (vec3-scale (phi-palette-color tileset-num 0) 0.2)
                      [128 128 128])
                        
        ]
    col
    )
  )

(defn draw []
  (reset! current-topology (topologies :rhombic-dodecahedron))
  (bezier-box-cache-reset)
  (if (< @state-idx (count in-states))
    (let [tiling (in-states @state-idx)
          tiling-rad (+ 0.0005 (get-assemblage-radius tiling))
          ;tiling (-> (make-params :max-iters 500
          ;                        :max-radius 32
          ;                        :tileset (get-random-tileset-1))
          ;           (make-state)
          ;           (make-tiling))
          params (tiling :params)
          attribs (-> default-render-attribs
                      (assoc :bbox-line-weight 0.75)
                      (assoc :bbox-faces? true)
                      (assoc :bbox-lines? true)
                      (assoc :bbox-res 16)
                      (assoc :simple-lines? false)
                      (assoc :boundary-mode :type-change)
                      )
          ;filename (format "output/%06d.png" (tiling :input-seq-id))
          filename (format "output/%06d.png" @state-idx)
;          filename (str "output/"
;                        (tileset-to-hex-number (params :tileset))
;                        "_S" (tilecode-to-hex-number (params :seed))
;                        ".png")
          cent (get-bounding-box-center tiling)
          ]

      (println @state-idx "of" (count in-states) ":"
               "tiles:" (count (tiling :tiles))
               "iters:" (tiling :iters) "/" (params :max-iters)
               "tileset:" (params :tileset)
               "radius:" (get-assemblage-radius tiling)
               "file:" filename)

      (when (and
              true
              ;(> (count (tiling :tiles)) 2)
              ;(= (tiling :solved) true)
              ; TODO: tiling uses all tiles in it's tileset
              ;(> @state-idx 4098)
              ;(not (.exists (as-file filename)))
              )
          
        (background 16 16 32)
        ;(apply background (get-bg-color (get-in tiling [:params :tileset])))
        ;(background 128 128 128)
        (position-camera tiling)
        ;(position-camera-ortho tiling)
        (setup-lights tiling)
        ;(lights)
        ;(rotate-x (/ Math/PI 2))
        ;(rotate-x (/ Math/PI 4))
        ;(rotate-z (/ Math/PI -4))

        ;(fill 0 192 32 127)
        ;(with-translation [(/ (width) 2) (/ (height) 2) 0]
          ;(scale (radius-to-scale2 tiling-rad))
          (scale mod-scale)
          ;(scale (+ 30.0
          ;          (* (/ 16.0 tiling-rad) 270.0)))
          ;(box 1.0 1.0 1.0)
          ;(fill 255)
          ;(sphere 32)
          
          ; t-rad = 1 ?
          ;  scale = 300
          ;t-rad =  16 ?
          ; scale = 32


          ;(draw-axes)
          (draw-tiling3 tiling attribs)
          (no-lights)
          (draw-empty2 tiling)
          ;)

          ;(draw-info)
          (save filename)
      )
      (swap! state-idx #(mod (inc %) (count in-states))))

    (do
      (println "done rendering")
      (no-loop)
      ;(sketch-close (qa/current-applet))
      )
    )
  )

;(defn draw []
;
;      (background 16 16 32)
;      (camera 0 10 0
;              0 0 0 
;              0 0 -1)
;      ;(position-camera-ortho tiling)
;      ;(setup-lights tiling)
;      ;(rotate-x (/ Math/PI 2))
;      ;(rotate-x (/ Math/PI 4))
;      ;(rotate-z (/ Math/PI -4))
;      ;(scale mod-scale)
;
;      ;(fill 0 192 32 127)
;      ;(with-translation cent
;      ;  (box 1.0 1.0 1.0))
;      ;(render tiling attribs filename)
;      
;      (fill 255)
;      (sphere 32)
;      ;(swap! state-idx inc)
;      
;      )

   
 


(defn -main [& args]
  (println "args:" args)
  (if (not= 1 (count args))
    (println "error: need <infile> arg")
    (do
      (def in-states (->> (load-all-tiler-states (first args))
                          ;(filter #(true? (% :solved)))
                          (sort-by #(apply str (get-in % [:params :tileset])))
                          (sort-by #(tileset-to-binary-number (get-in % [:params :tileset])) )
                           
                          ;(sort-by #(+ 
                          ;            (get-num-connected (first (get-in % [:params :tileset])))
                          ;            (get-num-connected (second (get-in % [:params :tileset])))))
                          
;                          (group-by #(tileset-to-binary-number (get-in % [:params :tileset])) )
;                          (vals)
;                          (mapcat (fn [g] (sort-by #(tilecode-to-binary-code (apply str (get-in % [:params :tileset]))) g)))

                          vec
                          ))
        (reset! bezier-box-control-bias 0.53)
        (reset! num-states (count in-states))
      (println "states loaded: " (count in-states))
      (defsketch rhombrick-render
          :setup setup
          :draw draw
          :size [1024 1024]
          :renderer :p3d))
    )
  )

;(-main "output-states-random-1024-all-seeds")
