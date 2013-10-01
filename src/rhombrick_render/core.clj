(ns rhombrick-render.core
  (:use [quil.core]
        [rhombrick.tilecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.offline]
        [rhombrick.vector]
        )
  (:require [quil.applet :as qa])
  (:gen-class :main true))

(def ^:dynamic in-states)
(def state-idx (atom 0))

(defn setup []
  (smooth)
  (def in-states (load-all-tiler-states "output-states-random-1024-all-seeds2"))
  )


(def mod-scale 1)


(defn position-camera-ortho [ts]
  (let [[mn1 mx1] (get-assemblage-extents ts)
        mg 1.0 ; margin
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
  (let [rad (+ 1 (get-assemblage-radius ts))
        d (* rad mod-scale 2) ]
    (camera 0 d 0
            0 0 0
            0 0 -1)))


(defn setup-lights [ts]
  ;(let [max-rad (+ 1 (get-assemblage-radius ts))
  (let [max-rad (get-in ts [:params :max-radius])
        r (/ max-rad 2.0)
        mr (/ (- max-rad) 2)]
    ;(lights)
    ;(ambient-light 32 32 32)
    (light-falloff 1.1 0.0 0.0)
    (spot-light 192 255 192 mr  r  r   1  -1  -1 (/ Math/PI 2) 2)
    (spot-light 255 192 192  r mr  r  -1   1  -1 (/ Math/PI 2) 2)
    (spot-light 192 192 255  r  r mr  -1  -1   1 (/ Math/PI 2) 2)))


(defn draw []
  (if (< @state-idx (count in-states))
    (let [tiling (in-states @state-idx)
          ;tiling (-> (make-params :max-iters 500
          ;                        :max-radius 32
          ;                        :tileset (get-random-tileset-1))
          ;           (make-state)
          ;           (make-tiling))
          params (tiling :params)
          attribs (-> default-render-attribs
                      (assoc :bbox-line-weight 0)
                      (assoc :bbox-lines? true)
                      (assoc :boundary-mode :all))
          filename (str "output/"
                        (tileset-to-hex-number (params :tileset))
                        "_S" (tilecode-to-hex-number (params :seed))
                        ".png")
          ;cent (get-bounding-box-center tiling)
          ]

      (println "tiles:" (count (tiling :tiles))
               "iters:" (tiling :iters) "/" (params :max-iters)
               "tileset:" (params :tileset)
               "radius:" (get-assemblage-radius tiling)
               "file:" filename)


      (when (> (count (tiling :tiles)) 2)
        (background 16 16 32)
        (position-camera-ortho tiling)
        (setup-lights tiling)
        ;(rotate-x (/ Math/PI 2))
        ;(rotate-x (/ Math/PI -4))
        ;(rotate-z (/ Math/PI -2))
        ;(scale mod-scale)

        ;(fill 0 192 32 127)
        ;(with-translation cent
        ;  (box 1.0 1.0 1.0))

        (render tiling attribs filename))
      (swap! state-idx inc))

    (do
      (println "done rendering")
      (no-loop)
      (sketch-close (qa/current-applet)))))
 


(defn -main [& args]
  (defsketch rhombrick-render
          :setup setup
          :draw draw
          :size [1100 1100]
          :renderer :p3d)
  )

