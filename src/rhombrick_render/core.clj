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
        [clojure.math.combinatorics]
        )
  )

(def ^:dynamic in-states)
(def state-idx (atom 0))
(def state-idx-prev (atom 0))

(def outfile-idx (atom 0))

(def num-states (atom 0))
(def camera-pos2 (atom [0.0 0.0 0.0]))
(def mod-scale 1.0)

(def ^:dynamic console-font)
(def particle-texture2 (atom nil))

(def rendered-tilesets (atom #{}))


(def ^:private ^:const hilbert-map
  {:a {'(0 0) '(0 :d) '(0 1) '(1 :a) '(1 0) '(3 :b) '(1 1) '(2 :a)}
   :b {'(0 0) '(2 :b) '(0 1) '(1 :b) '(1 0) '(3 :a) '(1 1) '(0 :c)}
   :c {'(0 0) '(2 :c) '(0 1) '(3 :d) '(1 0) '(1 :c) '(1 1) '(0 :b)}
   :d {'(0 0) '(0 :a) '(0 1) '(3 :c) '(1 0) '(1 :d) '(1 1) '(2 :d)}})

(defn xy->hilbert [x y lvl]
  (loop [sqr :a
         pos 0
         n (dec lvl)]
    (if (>= n 0)
      (let [qx (if (bit-test x n) 1 0)
            qy (if (bit-test y n) 1 0)
            [qpos sqr] (get-in hilbert-map [sqr (list qx qy)])
            pos (bit-or (bit-shift-left pos 2) qpos)]
        (recur sqr pos (dec n)))
      pos)))


(defn hilbert-plane [lvl]
  (let [n (bit-shift-left 1 lvl)]
    (sort-by
     #(xy->hilbert (first %) (second %) lvl)
     (for [x (range n) y (range n)] [x y]))))

(def hilbert-idxs (into [] (hilbert-plane 7)))


(defn seqid-to-xy [i xdim ydim]
  [ (mod i xdim) (int (Math/floor (/ i xdim))) ])


(defn seqid-to-hilbert [s xdim ydim]
  (.indexOf hilbert-idxs (seqid-to-xy s xdim ydim))
)


(defn get-hilbert-filename-id [out-idx xdim ydim]
  (let [hxy (hilbert-idxs out-idx)
        n (+ (hxy 0) (* (hxy 1) xdim))] ; x + y * xdim 
    n
  ))


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
  ;(frame-rate 0.5)
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




(defn get-preferred-cam-dir [exts]
  (let [
        axlens { [1 0 0] (+ 1 (Math/abs (- ((exts 0) 0) ((exts 1) 0) )))
                 [0 1 0] (+ 1 (Math/abs (- ((exts 0) 1) ((exts 1) 1) )))
                 [0 0 1] (+ 1 (Math/abs (- ((exts 0) 2) ((exts 1) 2) ))) }

        ax-sorted (sort-by val axlens)
        shortest-dir (key (first ax-sorted))
        shortest-dir (if (= shortest-dir [0 0 1])
                       (vec3-normalize [0 0.00001 1]) ; camera doesnt like to be
                                                      ;at [0 0 1] looking straight down
                       shortest-dir)
        ]
    ;(println "shortdir:" shortest-dir)
    shortest-dir
    ;(println "axis lengths:" ax-sorted)
    ;ax-sorted
      )
  )


(defn position-camera [ts]
  (let [fov (/ Math/PI 8.0)
        ;rad (+ 0.5 (get-assemblage-radius ts))
        rad (->> (get-tiling-dims ts)
                 (sort-by val)
                 (last)
                 (val)
                 ;#(/ % 2.0)
                 )
        max-rad (get-in ts [:params :max-radius])
        ;c (- 1.0 (/ (/ max-rad rad) max-rad))
        ;c (* rad 5.0)
        ;d (- 20.0 c)
        d 110

        ;d   (+ 0.0 (* c 100.0))
        p (vec3-scale (get-preferred-cam-dir (get-assemblage-extents ts))
                      d)
        c (get-bounding-box-center ts)

        ]
    ;(println "p:" p)
    ;(println "RAD:" rad "d:" d "c:" c)
    ;(reset! camera-pos2 [0.0 (* d (Math/sqrt 2.0)) 0.0])
    (reset! camera-pos2 [(p 0) (p 1) (p 2)])
    (perspective fov 1.0 0.1 500.0)
    (camera (p 0) (p 1) (p 2)
            (c 0) (c 1) (c 2)
            0 0 -1)))


(defn setup-lights [ts]
  ;(let [max-rad (+ 1 (get-assemblage-radius ts))
  (let [max-rad (get-in ts [:params :max-radius])
        r (* max-rad 4.0)
        mr (/ (- max-rad) 2)]
        ;mr (/ max-rad 2)]
    ;(lights)
    ;(ambient-light 32 32 32)
    ;(light-falloff 1.1 0.0 0.0)
    (spot-light 192 255 192 mr  (+ r) (+ r)   1  -1  -1 (/ Math/PI 2) 2)
    (spot-light 255 192 192  r  (+ mr) (+ r)  -1  1  -1 (/ Math/PI 2) 2)
    (spot-light 192 192 255  r  (+ r) (+ mr)  -1  -1  1 (/ Math/PI 2) 2)))


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
      (scale 1.0)
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


(defn draw-centers [ts]
  (let [c (get-bounding-box-center ts)]
    (stroke-weight 1.0)
    (no-fill)
    (stroke 255 255 64 192)
    (box 0.5)

    (stroke 255 64 128 192)
    (with-translation (vec3-scale c -1.0)
      (scale 0.5)
      (box 1 1 1))))


;(defn radius-to-scale [r]
;  (let [tiling-max-rad 8.0
;        rnorm (/ r tiling-max-rad)]
;    (+ 53.0 
;       (* (- 1.0 rnorm)
;          100.0))))
        

;(defn radius-to-scale2 [r]
;  (let [tiling-max-rad 8.0
;        rnorm (/ r tiling-max-rad)]
;    (+ 10.0 
;       (* (- 1.0 rnorm)
;          100.0))))
  

(defn get-scale-factor [ts]
  (let [;r (+ 0.0 (get-assemblage-radius ts))
        r (->> (get-tiling-dims ts)
               (sort-by val)
               (last)
               (val)
                )
        r (+ -0.666 (/ r 2.0))
        max-rad (get-in ts [:params :max-radius])
        sf (+ 1.0 (/ max-rad r))
        ;sf (* sf sf)
        ]
    ;(println "scale-factor:" sf)
    sf
  ))


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
          ;tileset (normalize-tileset (get-in tiling [:params :tileset]))
          tileset (normalize-tileset (get-in tiling [:params :tileset]))
          ;tiling-rad (+ 0.0005 (get-assemblage-radius tiling))
          ;extents (get-assemblage-extents tiling)
          ;shortdir (get-preferred-cam-pos extents)
          tiling-scale (get-scale-factor tiling)
          ;tiling (-> (make-params :max-iters 500
          ;                        :max-radius 32
          ;                        :tileset (get-random-tileset-1))
          ;           (make-state)
          ;           (make-tiling))
          params (tiling :params)
          attribs (-> default-render-attribs
                      (assoc :bbox-line-weight 0.75)
                      (assoc :bbox-faces? true)
                      (assoc :bbox-lines? false)
                      (assoc :bbox-res 16)
                      (assoc :simple-lines? false)
                      ;(assoc :boundary-mode :type-change)
                      (assoc :boundary-mode :all)
                      )
          ;filename (format "output/%06d.png" (tiling :input-seq-id))
          ; hilb (seqid-to-hilbert @outfile-idx 64 64)
          hilb (get-hilbert-filename-id @outfile-idx 128 128)
          dirnum (* 1024 (int (Math/floor (/ hilb 1024))))
          filename (format "output/%08d/%08d.png" dirnum hilb)
          ;filename (format "output/%06d.png" @outfile-idx)
;          filename (str "output/"
;                        (tileset-to-hex-number (params :tileset))
;                        "_S" (tilecode-to-hex-number (params :seed))
;                        ".png")
          cent (get-bounding-box-center tiling)
          ]
      (when (not= @state-idx-prev @state-idx)
        (reset! state-idx-prev @state-idx)
        (println @state-idx "of" (count in-states) ":"
                 "tiles:" (count (tiling :tiles))
                 "iters:" (tiling :iters) "/" (params :max-iters)
                 "tileset:" (params :tileset)
                 "radius:" (get-assemblage-radius tiling)
                 "file:" filename)
      )

      (when (and
              ;true
              ;(= (count tileset) 2)
              ;(> (count (tiling :tiles)) 2)
              ;(= (tiling :solved) true)
              ; TODO: tiling uses all tiles in it's tileset
              ;(< @outfile-idx 4096)
              ;(not (.exists (as-file filename)))
              (not (@rendered-tilesets tileset))
              ;(not (zero? (compare (first tileset) "------------")))
              ;(not (zero? (compare (second tileset) "------------")))
              
              ;(or ; reject tilesets that dont have at least one tile with at least 3 conns 
              ;  (> (get-num-connected (first tileset)) 2)
              ;  (> (get-num-connected (second tileset)) 2))
              )
              
        
        (swap! rendered-tilesets conj rendered-tilesets tileset)

        (background 16 16 32)
        ;(apply background (get-bg-color (get-in tiling [:params :tileset])))
        ;(background 128 128 128)
        (position-camera tiling)
        ;(position-camera-ortho tiling)
        (setup-lights tiling)
        ;(lights)
        ;(rotate-x (/ Math/PI 2))
        ;(rotate-z (/ Math/PI 4))
        ;(rotate-x (/ Math/PI 4))
        ;(rotate-z (* (frame-count) 0.01))
        ;(fill 0 192 32 127)
        
        ;(draw-axes)
        ;(println "TILING SCALE:" tiling-scale "TRANSLATE:" (vec3-scale cent (* tiling-scale -1.0)))

        ;(with-translation cent
        (with-translation (vec3-scale cent (* tiling-scale -1.0))
        ;(with-translation [(/ (width) 2) (/ (height) 2) 0]
          ;(scale (radius-to-scale2 tiling-rad))
          (scale tiling-scale)
          ;(scale mod-scale)
          ;(scale (+ 30.0
          ;          (* (/ 16.0 tiling-rad) 270.0)))
          ;(box 1.0 1.0 1.0)
          ;(fill 255)
          ;(sphere 32)
          
          ; t-rad = 1 ?
          ;  scale = 300
          ;t-rad =  16 ?
          ; scale = 32

          ;(build-face-list-ts tiling)
          ;(draw-face-list)
          ;(with-rotation [(/ Math/PI 4.0) 1 0 0]
          ;  (rotate-z (/ Math/PI 2))
            (draw-tiling3 tiling attribs)
          ;  )
          ;(no-lights)
          ;(draw-empty2 tiling)

          ;)
        )
        
        (draw-info)

        ;(scale tiling-scale)
        ;(draw-aabb tiling)
        ;(draw-centers tiling)
        (when true ;(not (.exists (as-file filename)))
          (save filename)
          (swap! outfile-idx inc)
          )
      )
      (swap! state-idx inc)
      ;(swap! state-idx #(mod (inc %) (count in-states)))
      ;(reset! rendered-tilesets #{})
    )
      ;(swap! state-idx #(mod (inc %) 2)))

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

 
(def key-command-map
  {
   \n #(do
         (swap! state-idx inc)
         ;(println "tiling bounding box: " (get-assemblage-extents @tiler-state))
         (println "model-scale: " @model-scale)
         )
   })


(defn key-typed []
  (let [keychar (raw-key)]
    (when (contains? key-command-map keychar)
      ((key-command-map keychar)))))


(defn tiling-balanced? [ts]
  (let [min-ratio 0.1
        tiles-norm (->> (vals (ts :tiles)) ; (map #(second (val %) (ts :tiles))
                        (map normalize-tilecode)
                        )
        freqs (frequencies tiles-norm)
        tiles-forms (keys freqs)
        sel-forms (selections tiles-forms 2)
        ratios (map #(/ (+ (freqs (first %)) 0.00000001)
                        (+ (freqs (second %)) 0.00000001))
                    sel-forms)
        answer (and (= (count tiles-forms)
                       (count (get-in ts [:params :tileset])))
                    (zero? (count (filter #(< % min-ratio) ratios))))]
    answer))

; get counts of tileset connections
; (map key (group-by get-tileset-con-counts rd-twotiles-tilesets))
;(map #(vec [(key %) (count (val %))])
;                             (group-by get-tileset-con-counts rd-twotiles-tilesets))

;(defn get-tileset-con-counts [tileset]
;    (into #{} (map get-num-connected tileset))
;  )

(defn get-tilesets-con-group-counts [states]
  (into {}
    (map #(vec [(key %) (count (val %))])
                             (group-by #(get-tileset-con-counts (get-in % [:params :tileset]))
                                       states))))


;  (let [groups (map key (group-by #(set (get-tileset-con-counts %)) tilesets))
;        groups2 (map #(vec [(key %)  ]) groups)
;        ]
;  ))


(defn validate-state [ts]
  (let [tileset (get-in ts [:params :tileset])]
    (and
      ;true
      (= (count tileset) 2)
      (> (count (ts :tiles)) 2)
      (= (ts :solved) true)
      (tiling-balanced? ts)
      ; TODO: tiling uses all tiles in it's tileset
      ;(< @outfile-idx 4096)
      ;(not (.exists (as-file filename)))
      ;(not (@rendered-tilesets tileset))
      (not (zero? (compare (first tileset) "------------")))
      (not (zero? (compare (second tileset) "------------")))
 
      (or ; reject tilesets that dont have at least one tile with at least 3 conns
        (> (get-num-connected (first tileset)) 2)
        (> (get-num-connected (second tileset)) 2))
      )
  ))


(defn get-tile-sym-counts [tilecode]
  (into {} (map #(vec [(key %) (count (val %))])
                (group-by identity (get-tilecode-angles tilecode)))))

(defn get-tileset-sym-counts [tileset]
  (into #{} (map get-tile-sym-counts tileset))
  )

(defn get-tilesets-sym-group-counts [states]
  (into {}
    (map #(vec [(key %) (count (val %))])
                             (group-by #(get-tileset-sym-counts (get-in % [:params :tileset]))
                                       states))))

(defn -main [& args]
  (println "args:" args)
  (if (not= 1 (count args))
    (println "error: need <infile> arg")
    (let [states (filter validate-state (load-all-tiler-states (first args)))
          group-counts (get-tilesets-con-group-counts states)
          sym-group-counts (get-tilesets-sym-group-counts states)

          ]
      (println "loading input states from" (first args))
      (def in-states (->> states ; (load-all-tiler-states (first args))
                          ;(filter #(true? (% :solved)))
                          ;(filter tiling-balanced?)
                          ;(filter #(validate-state %))
                          ;(filter #(< (count (% :tiles)) 64 ))
 
                          ;(sort-by #(apply str (get-in % [:params :tileset])))
                          ;(sort-by #(tileset-to-binary-number (get-in % [:params :tileset])) )
                           
                          ;(sort-by #(+ 
                          ;            (get-num-connected (first (get-in % [:params :tileset])))
                          ;            (get-num-connected (second (get-in % [:params :tileset])))))
                          
                          (group-by #(get-tileset-sym-counts (get-in % [:params :tileset])) )
                          ;(group-by #(get-tileset-con-counts (get-in % [:params :tileset])) )
                          ;(group-by #(tileset-cons-hash (get-in % [:params :tileset])) )
                          (sort-by #(sym-group-counts (key %)) )
                          (reverse)
                          ;(sort-by #(reduce + (map (fn [n] (* n n)) (key %))) )
                          ;(sort-by #(reduce + (key %)))
                          ;(group-by #(* (get-num-connected (first (get-in % [:params :tileset])))
                          ;              (get-num-connected (second (get-in % [:params :tileset])))) )
                          (vals)
                          ;(mapcat (fn [g] (sort-by #(tilecode-to-binary-code (apply str (get-in % [:params :tileset]))) g)))
                          (mapcat (fn [ts] (sort-by #(count (% :tiles)) ts)) )
                          ;(mapcat (fn [g] (sort-by #(apply str (normalize-tileset (get-in % [:params :tileset]))) g)))

                          vec
                          ))
        (reset! bezier-box-control-bias 0.53)
        (reset! num-states (count in-states))
        (reset! outfile-idx 0)
        (reset! rendered-tilesets #{})
      (println "states loaded: " @num-states)
      (defsketch rhombrick-render
          :setup setup
          :draw draw
          :size [512 512]
          ;:size [1024 1024]
          :renderer :p3d
          :key-typed key-typed))
    )
  )

;(-main "output-states-random-1024-all-seeds")


;(doseq [i (range 256)]
;  (println "java -jar rd-twotile-random-32768.state rd-twotile-random-32768.tiling"
;           (* i 128) (+ (* i 128) 128)))
