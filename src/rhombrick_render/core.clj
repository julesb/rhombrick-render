(ns rhombrick-render.core
  (:use [quil.core]
        [rhombrick.tiling]
        [rhombrick.tilecode]
        [rhombrick.tiling-render]
        [rhombrick.offline]
        )
  (:gen-class :main true))



(defn setup []
  (smooth)
  ;(no-loop)
  )



(defn draw []
  (background 32 32 64)
  (smooth)
  (camera 100 0 0
            0 0 0
            0 0 -1)
  (lights)
  (scale 10)
  (render (-> (make-params :max-iters 1000
                           :tileset (get-random-tileset))
              (make-state)
              (make-tiling))

          (-> default-render-attribs
              (assoc :bbox-line-weight 2)
              (assoc :bbox-lines? true)
              (assoc :boundary-mode :all))
          
          "tiling.png"))
 


(defn -main [& args]
  (sketch :setup setup
          :draw draw
          :size [800 800]
          :renderer :p3d)
  )

