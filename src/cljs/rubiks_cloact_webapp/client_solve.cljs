(ns rubiks-cloact-webapp.client-solve
  (:require [rubiks-cloact-webapp.solver :as s]))


(defn orientation-from-face-rep [frcs]
  (into {} (map (fn [[k v]] [k (get-in v [1 1])]) frcs)))

(defn apply-algorithm [frcs moves]
  (let [ormp (orientation-from-face-rep frcs)
        rcs (s/from-faces frcs)
        trcs (s/apply-algorithm rcs (map second moves))]
    (s/to-faces trcs ormp)))
(defn dr
  ([x] (js/console.log (clj->js x)) x)
  ([txt x] (js/console.log txt) (dr x)))
(defn solve-rubiks-cube [frcs]
  (js/console.log "from client solve")
  (js/console.log frcs)
  (->> frcs (dr "before from-faces") s/from-faces (dr "before solve") s/solve (dr "before meta") meta (dr "before moves-applied") :moves-applied (map-indexed #(vector %1 %2))))
(defn random-rubiks-cube
  ([ormp] (s/to-faces (s/scrambled-rubiks-cube) ormp))
  ([] (s/to-faces (s/scrambled-rubiks-cube))))
