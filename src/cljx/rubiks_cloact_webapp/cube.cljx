(ns rubiks-cloact-webapp.cube
  (:require [clojure.set]))

(defn cartesian-product
  ([] [])
  ([x] (for [xi x] [xi]))
  ([x y] (for [xi x yi y] [xi yi]))
  ([x y z] (for [xi x yi y zi z] [xi yi zi])))

(defn combinations [[x & xs] n]
  (cond
   (= n 0) [[]]
   (nil? x) []
   :default (concat (map #(cons x %) (combinations xs (dec n)))
                    (combinations xs n))))


(defn cube-geometry [size]
  (let [half-size (/ size 2)
        dirs [:x :y :z]
        dir-pairs [[[:x :y] :z] [[:y :z] :x] [[:z :x] :y]]
        coord-map-to-coord-vec (fn [{:keys [x y z] :or {x 0 y 0 z 0}}] [x y z])
        cvals [(- half-size) half-size]
        dir-faces (fn [[[d1 d2] d3]]
                    (let [[v00 v01 v10 v11] (for [c-v1 cvals c-v2 cvals] {d1 c-v1 d2 c-v2})
                          f-coords [v00 v10 v11 v01]
                          positive-face (mapv #(coord-map-to-coord-vec (assoc % d3 (cvals 0))) f-coords)
                          negative-face (mapv #(coord-map-to-coord-vec (assoc % d3 (cvals 1))) (rseq f-coords))]
                      [d3 {:positive-face positive-face :negative-face negative-face}]))
        faces (into {} (map dir-faces dir-pairs))]
    faces))

(defn cube-piece [{:keys [x y z] :as p} n]
  (let [cgeom (cube-geometry (/ 1 n))]
    {:type :translate :x x :y y :z z
     :nodes (mapcat
             (fn [[dir [dir-coord dir-color]]]
               (let [{:keys [positive-face negative-face]} (cgeom dir)]
                (cond
                 (= dir-coord n) [{:face positive-face :color dir-color} {:face negative-face}]
                 (= dir-coord (- n)) [{:face positive-face} {:face negative-face :color dir-color}]
                 :default [{:face positive-face} {:face negative-face}])))
             p)}))
(def dirs [:x :y :z])
(def sides [[:green :blue] [:white :yellow] [:red :orange]])

(defn rubiks-cube-nxnxn [n]
  (let [pairs-to-map #(into {} %)
        boundary-sides (map (fn [dir [neg-color pos-color]] [[dir [0 neg-color]] [dir [(dec n) pos-color]]]) dirs sides)
        corner-pieces (apply cartesian-product boundary-sides)
        edge-pieces (mapcat #(apply cartesian-product %) (combinations boundary-sides 2))
        face-pieces (map vector (apply concat boundary-sides))
        dirs-set (set dirs)
        general-pieces (concat corner-pieces edge-pieces face-pieces)]
    (mapcat (fn [general-piece]
           (let [bound-dirs (map first general-piece)
                 dirs-to-iter (apply disj dirs-set bound-dirs)
                 vec-range (map vector (range 1 (dec n)))
                 general-piece-map (pairs-to-map general-piece)
                 piece-variables-before-cp (map (fn [dir]
                                                  (mapv #(vector dir %)
                                                        vec-range))
                                                dirs-to-iter)
                 piece-variables (or (seq (apply cartesian-product piece-variables-before-cp)) [[]])
                 ret (map #(into general-piece-map %) piece-variables)]
             ret))
         general-pieces)))
#_ (rubiks-cube-nxnxn 2)
