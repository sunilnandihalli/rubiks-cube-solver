(ns rubiks-cloact-webapp.solver
  (:require [clojure.set]))

(def sides [[:green :blue] [:white :yellow] [:red :orange]])


(def human-comprehendible-sides [:front :right :left :up :down :back])

(let [opp-color (into {} (concat sides (map (fn [[x y]] [y x]) sides)))]
  (defn opposite [color] (opp-color color)))

(let [x [:clockwise :counter-clockwise]
      opp-dir (into {} [x (vec (reverse x))])]
  (defn opposite-orientation [orientation]
    (opp-dir orientation)))

(let [start-colors (set (map first sides))]
  (defn orientation-colors
    ([color] (vec (orientation-colors color :clockwise)))
    ([color orientation]
       (vec (if (start-colors color)
              (if (not= orientation :clockwise) (reverse (orientation-colors color :clockwise))
                  (let [[[a b] [c d]] (take 2 (drop 1 (drop-while #(not= (first %) color) (cycle sides))))]
                    [a c b d]))
              (orientation-colors (opposite color) (opposite-orientation orientation)))))))

(defn next-color
  ([rot-color cur-color] (next-color rot-color cur-color :clockwise))
  ([rot-color cur-color rot-dir]
     (second (drop-while #(not= cur-color %) (cycle (orientation-colors rot-color rot-dir))))))

(let [transition-map (into {} (for [x (apply concat sides) y [:clockwise :counter-clockwise]]
                                [[x y] (let [oclrs (orientation-colors x y)]
                                         (into {x x} (map vector oclrs (drop 1 (cycle oclrs)))))]))]
  (defn rotate-rubiks-cube [rubiks-cube-state & [color orientation :as move]]
    (let [{:keys [original-cube] :as m} (meta rubiks-cube-state)
          cur-transition-map (transition-map move)]
      (with-meta (into {} (map (fn [[k v]]
                                 [k (if (v color)
                                      (into {} (map (fn [[side-color actual-color]] [(cur-transition-map side-color) actual-color]) v)) v)])
                               rubiks-cube-state))
        (update-in (if original-cube m {:original-cube rubiks-cube-state :moves-applied []}) [:moves-applied] conj move)))))

(defn full-orientation [ormp]
  (loop [{:keys [top front bottom right left back] :as fr} ormp]
    (if (and top front bottom right left back) fr
        (recur (apply assoc fr
                      (cond
                       (and front (nil? back)) [:back (opposite front)]
                       (and back (nil? front)) [:front (opposite back)]
                       (and right (nil? left)) [:left (opposite right)]
                       (and left (nil? right)) [:right (opposite left)]
                       (and top (nil? bottom)) [:bottom (opposite top)]
                       (and bottom (nil? top)) [:top (opposite bottom)]
                       (and front right) [:top (second (drop-while #(not= left %) (cycle (orientation-colors front))))]
                       (and front bottom) [:right (second (drop-while #(not= top %) (cycle (orientation-colors front))))]
                       (and right bottom) [:front (second (drop-while #(not= bottom %) (cycle (orientation-colors right))))]
                       (and front (every? nil? [top right])) [:top (first (orientation-colors front))]
                       (and right (every? nil? [top front])) [:top (first (orientation-colors right))]
                       (and top (every? nil? [right front])) [:right (first (orientation-colors top))]
                       (nil? front) [:front :green]))))))

(let [dir-ort-map {:x [:left :right] :y [:bottom :top] :z [:back :front]}]
 (defn from-generic-state [{:keys [pieces n]}]
   {:pre [(= n 3)]}
   (let [ormp (into {} (keep (fn [{[x xc] :x [y yc] :y [z zc] :z :as piece}]
                               (case [x y z] ; ideal candidate for core.match?
                                 [0 1 1] [:left xc]
                                 [2 1 1] [:right xc]
                                 [1 0 1] [:bottom yc]
                                 [1 2 1] [:top yc]
                                 [1 1 0] [:back zc]
                                 [1 1 2] [:front zc]
                                 nil)) pieces))]
     {:orientation ormp
      :rubiks-cube-state (into {}
                               (map (fn [general-piece]
                                         (let [c (keep (fn [[dir [coord color]]]
                                                         (if color
                                                           (let [[min max] (dir-ort-map dir)]
                                                             [(ormp (case coord 0 min 2 max)) color])))
                                                       general-piece)
                                               piece (set (map second c))]
                                           [piece (into {} c)]))
                                    pieces))}))

 (defn to-generic-state
   ([rcs ormp]
      (let [default-generic-piece {:x [1] :y [1] :z [1]}
            dir-to-generic-coord {:left [:x [0]] :right [:x [2]] :bottom [:y [0]] :top [:y [2]] :back [:z [0]] :front [:z [2]]}
            color-to-generic-coord (into {} (map (fn [[k v]] [(ormp k) v]) dir-to-generic-coord))
            center-face-pieces (map (fn [[color [dir [coord]]]]
                                      (assoc default-generic-piece dir [coord color]))
                                    color-to-generic-coord)]
        {:n 3
         :pieces (map (fn [[piece dir-color-map]]
                        (reduce (fn [cur-generic-piece [dir-color actual-color]]
                                  (let [[dir [coord]] (color-to-generic-coord dir-color)]
                                    (assoc cur-generic-piece dir [coord actual-color])))
                                default-generic-piece dir-color-map ))
                      rcs)}))
   ([rcs] (to-generic-state rcs (full-orientation {})))))


(let [char-orientation-map {\U :top \R :right \L :left \F :front \D :bottom \B :back}]
  (defn ops-to-string [ops ormp]
    (let [cm (into {} (map (fn [[c ort]] [(ormp ort) c]) char-orientation-map))]
      (apply str (interpose " " (map (fn [[c rot-dir]] (str (cm c)
                                                           (case rot-dir :clockwise "" :counter-clockwise "i"))) ops)))))
  (defn string-to-ops [algo-string ormp]
    (map (fn [[a b]] [(-> a char-orientation-map ormp) (if b :counter-clockwise :clockwise)])
         (clojure.string/split algo-string #"\s+")))
  (defn apply-algorithm
    ([rubiks-cube-state algo-string orientation-map]
       (let [ormp (full-orientation orientation-map)
             ops (string-to-ops algo-string ormp)]
         (apply-algorithm rubiks-cube-state ops)))
    ([rubiks-cube-state ops]
       (reduce (fn [s [c o]] (rotate-rubiks-cube s c o)) rubiks-cube-state ops))))


(defn transform-rubiks-cube [rubiks-cube-state {:keys [piece f-pos i-pos rot-dir]}]
  (if (= i-pos f-pos) rubiks-cube-state
      (let [i-pos (if i-pos i-pos (set (keys (rubiks-cube-state piece))))
            rot-dir (let [x (clojure.set/intersection i-pos f-pos)]
                      (if (= 1 (count x)) (first x)
                          (if rot-dir rot-dir
                              (print "please specify rot-dir : " rot-dir " x : " x " f-pos : " f-pos " i-pos : " i-pos))))
            [i-pos-r f-pos-r] (map #(disj % rot-dir) [i-pos f-pos])
            dist (->> (cycle (orientation-colors rot-dir))
                      (partition (count i-pos-r) 1)
                      (map set)
                      (drop-while #(not= % i-pos-r))
                      (take-while #(not= % f-pos-r))
                      count)]
        (apply-algorithm rubiks-cube-state (repeat dist [rot-dir :clockwise])))))

(defn current-location [rubiks-cube-state piece]
  (set (keys (rubiks-cube-state piece))))

(defn solve-first-layer-cross [rubiks-cube-state c s-c]
  (let [[o-c o-s-c] (map opposite [c s-c])
        [piece opp-loc] [#{c s-c} #{o-c s-c}]]
    (loop [r-c-s rubiks-cube-state]
      (let [cur-loc (current-location r-c-s piece)]
        (if (= cur-loc piece) r-c-s
            (if-let [f-pos (cond
                            (= opp-loc cur-loc) piece
                            (cur-loc o-c) opp-loc
                            (cur-loc c) (conj (disj cur-loc c) o-c)
                            (cur-loc s-c) piece
                            :default nil)]
              (recur (transform-rubiks-cube r-c-s {:i-pos cur-loc :f-pos f-pos}))
              (if (cur-loc o-s-c)
                (let [other-dir (first (disj cur-loc o-s-c))
                      nxt-clr (next-color o-s-c other-dir :clockwise)
                      algo-str (if (= c nxt-clr) "U U F U U" "U U Fi U U")]
                  (recur (apply-algorithm r-c-s algo-str {:top c :front o-s-c})))
                (throw :should-not-come-here))))))))
(defn is-piece-correctly-placed-and-oriented [rubiks-cube-state piece]
  (let [cur-loc (current-location rubiks-cube-state piece)]
    (and (= cur-loc piece) (every? #(apply = %) (rubiks-cube-state piece)))))
(defn is-piece-correctly-placed [r-c-s piece]
  (= (current-location r-c-s piece) piece))

(defn orientation-color-pairs
  ([c] (orientation-color-pairs c :clockwise))
  ([c ort-dir] (take 4 (partition 2 1 (cycle (orientation-colors c ort-dir))))))
(defn first-layer-cross [c] (map #(set [c %]) (orientation-colors c)))
(defn first-layer-corner [c] (map #(set (conj % c)) (orientation-color-pairs c)))
(defn second-layer-corner [c] (map set (orientation-color-pairs c)))
(defn third-layer-cross [c] (let [o-c (opposite c)]
                                    (map #(set [o-c %]) (orientation-colors c))))
(defn third-layer-corner [c] (let [o-c (opposite c)] (map #(set (conj % o-c)) (orientation-color-pairs c))))

(defn verify [locs locs-and-orts r-c-s c]
  (when-not (and
             (every? #(is-piece-correctly-placed-and-oriented r-c-s %) (mapcat #(% c) locs-and-orts))
             (every? #(is-piece-correctly-placed r-c-s %) (mapcat #(% c) locs)))
    (assert nil (str (apply str "locs : " locs)
                     (apply str "locs-and-orts : " locs-and-orts)))))

(defn solve-first-layer-corner [rubiks-cube-state top [right front]]
  (let [piece #{top right front} opp-piece #{(opposite top) right front}
        cur-loc (current-location rubiks-cube-state piece)
        {:keys [top right front left back bottom] :as ormp} (full-orientation {:top top :right right})
        correct-or-opposite-to-correct-state (loop [r-c-s rubiks-cube-state]
                                               (let [cur-loc (current-location r-c-s piece)]
                                                 (cond
                                                  (#{opp-piece piece} cur-loc) r-c-s
                                                  (cur-loc top) (recur (let [ort {:top top
                                                                                  :front (some #(cur-loc (next-color top %))
                                                                                               (disj cur-loc top))}]
                                                                         (apply-algorithm r-c-s "Ri Di R" ort)))
                                                  :default (let [trf-param {:i-pos cur-loc :f-pos opp-piece :rot-dir bottom}]
                                                             (transform-rubiks-cube r-c-s trf-param)))))]
    (loop [r-c-s correct-or-opposite-to-correct-state]
      (if (is-piece-correctly-placed-and-oriented r-c-s piece) r-c-s
          (recur (apply-algorithm r-c-s "Ri Di R D" ormp))))))


(defn solve
  ([rubiks-cube-state]
     (solve rubiks-cube-state :green))
  ([rubiks-cube-state c]
     (let [oclrs (orientation-colors c)
           o-c (opposite c)
           dir-pairs (orientation-color-pairs c)
           unoriented-correct-first-layer-cross (reduce #(solve-first-layer-cross %1 c %2) rubiks-cube-state oclrs)
           re-orient-first-layer-cross-tips (fn [r-c-s side]
                                              (if (apply = (first (r-c-s #{c side}))) r-c-s
                                                  (apply-algorithm r-c-s "Fi U Li Ui" {:front side :top c})))
           correct-first-layer-cross-tips (reduce re-orient-first-layer-cross-tips unoriented-correct-first-layer-cross oclrs)
           top-layer-correct (reduce #(solve-first-layer-corner %1 c %2) correct-first-layer-cross-tips dir-pairs)

           fix-middle-layer (fn [r-c-s piece]
                              (let [ort (r-c-s piece)]
                                (if (ort o-c)
                                  (let [[cur-color to-color] (first (dissoc ort o-c))
                                        r-c-s-1 (transform-rubiks-cube r-c-s {:i-pos #{cur-color o-c} :f-pos #{to-color o-c}})
                                        [matched-color tobe-matched-color] [to-color ((r-c-s-1 piece) o-c)]
                                        [R _ L] (first (filter #(= (second %) matched-color) (partition 3 1 (cycle (reverse oclrs)))))
                                        rotation-algo (cond
                                                       (= R tobe-matched-color) "U R Ui Ri Ui Fi U F"
                                                       (= L tobe-matched-color) "Ui Li U L U F Ui Fi"
                                                       :default (throw :should-not-come-here))]
                                    (apply-algorithm r-c-s-1 rotation-algo {:front matched-color :top o-c}))
                                  (let [loc (set (keys ort))
                                        front (some #(loc (next-color o-c %)) loc)]
                                    (recur (apply-algorithm r-c-s "U R Ui Ri Ui Fi U F" {:front front :top o-c}) piece)))))
           correct-middle-layer (reduce fix-middle-layer top-layer-correct (map set dir-pairs))
           correct-top-layer-cross (loop [r-c-s correct-middle-layer]
                                     (let [correct-top-face-dirs (keep #(let [ort (r-c-s #{o-c %})]
                                                                          (if (= (ort o-c) o-c) (ffirst (dissoc ort o-c)))) (rseq oclrs))
                                           dir-set (set correct-top-face-dirs)]
                                       (if (= (count dir-set) 4) r-c-s
                                           (if-let [f (some #(if (dir-set (opposite %)) (next-color o-c %)) dir-set)]
                                             (recur (apply-algorithm r-c-s "F R U Ri Ui Fi" {:top o-c :front f}))
                                             (let [f (some #(if-let [x (dir-set (next-color o-c %))] (opposite x)) dir-set)]
                                               (recur (apply-algorithm r-c-s "F U R Ui Ri Fi" {:top o-c :front f})))))))
           correct-top-layer-cross-rightly-aligned (loop [r-c-s correct-top-layer-cross]
                                                     (let [top-cross-colors (-> (into {}
                                                                                      (map #(first (dissoc (r-c-s #{% o-c}) o-c)) (rseq oclrs)))
                                                                                (map (rseq oclrs)))
                                                           to-back (ffirst (or (seq (filter (fn [[back right]] (= (next-color o-c back) right))
                                                                                            (take 4 (partition 2 1 (cycle top-cross-colors)))))
                                                                               (seq (filter (fn [[back _ front]] (= (opposite back) front))
                                                                                            (take 4 (partition 3 1 (cycle top-cross-colors)))))))
                                                           r-c-s-1 (transform-rubiks-cube r-c-s {:i-pos (current-location r-c-s #{o-c to-back})
                                                                                                 :f-pos #{o-c to-back}})]
                                                       (if (every? #(apply = (first (dissoc (r-c-s-1 #{% o-c}) o-c))) (rseq oclrs)) r-c-s-1
                                                           (recur (apply-algorithm r-c-s-1 "R U Ri U R U U Ri" {:back to-back :top o-c})))))
           top-layer-corner-pieces (map #(set (cons o-c %)) dir-pairs)
           correct-top-layer-corner-rightly-placed (loop [r-c-s correct-top-layer-cross-rightly-aligned]
                                                     (if (every? #(is-piece-correctly-placed r-c-s %) top-layer-corner-pieces) r-c-s
                                                         (let [correct-top-corner-piece (some #(if (is-piece-correctly-placed r-c-s %) %)
                                                                                              top-layer-corner-pieces)
                                                               ormp {:top o-c :front (some #(correct-top-corner-piece (next-color o-c %))
                                                                                           (disj correct-top-corner-piece o-c))}]
                                                           (recur (apply-algorithm r-c-s "U R Ui Li U Ri Ui L" ormp)))))
           solved-cube (if-let [incorrect-piece (some #(if-not (is-piece-correctly-placed-and-oriented correct-top-layer-corner-rightly-placed %) %)
                                                      top-layer-corner-pieces)]
                         (let [front (some #(incorrect-piece (next-color o-c %)) (disj incorrect-piece o-c))
                               {:keys [top right front left bottom back] :as ormp} (full-orientation {:top o-c :front front})
                               incorrect-right-left (disj incorrect-piece o-c)
                               sequence-of-right-front-colors (take 4 (drop-while #(not= (set %) incorrect-right-left)
                                                                                  (partition 2 1 (cycle (rseq oclrs)))))]
                           (loop [r-c-s correct-top-layer-corner-rightly-placed
                                  [[right-color front-color :as rf] & rest-of-rf-clrs :as whole-rf-clrs] sequence-of-right-front-colors]
                             (if-not rf r-c-s
                                     (let [p (set (cons o-c rf))]
                                       (if (let [ort (r-c-s p)]
                                             (and (= (ort o-c) o-c) (= (ort right) right-color) (= (ort front) front-color)))
                                         (recur (apply-algorithm r-c-s "Ui" ormp) rest-of-rf-clrs)
                                         (recur (apply-algorithm r-c-s "Ri Di R D" ormp) whole-rf-clrs))))))
                         correct-top-layer-corner-rightly-placed)]
       solved-cube)))

(defn rev-algo [s]
  (apply str (interpose " " (map (fn [[a b]] (if b (str a) (str a \i))) (rseq (clojure.string/split s #"\s+"))))))

(defn solve-rubiks-cube [grcs]
  (let [{:keys [pieces n]} grcs
        {rcs :rubiks-cube-state ormp :orientation} (from-generic-state grcs)
        color-ort-to-dir-coord-ort (let [rev-ormp (clojure.set/map-invert ormp)]
                                     (fn [[clr ort]]
                                       (case (rev-ormp clr)
                                         :left   [:x [0 (opposite-orientation ort)]]
                                         :right  [:x [2 ort]]
                                         :bottom [:y [0 (opposite-orientation ort)]]
                                         :top    [:y [2 ort]]
                                         :back   [:z [0 (opposite-orientation ort)]]
                                         :front  [:z [2 ort]])))
        solution (->> rcs solve meta :moves-applied (map-indexed (fn [move-id clr-ort]
                                                                   [move-id (color-ort-to-dir-coord-ort clr-ort)])))]
    solution))
