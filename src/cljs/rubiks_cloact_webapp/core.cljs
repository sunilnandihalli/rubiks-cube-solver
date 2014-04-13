(ns rubiks-cloact-webapp.core
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :as async :refer [onto-chan put! chan alts! <! >! timeout close!]]
   [rubiks-cloact-webapp.solver :as s]
   [rubiks-cloact-webapp.cube :as c]
   [reagent.core :as reagent :refer [atom]]))

;; Lets you do (prn "stuff") to the console
(enable-console-print!)
(defn abs [x] (if (< x 0) (- x) x))
(defn new-random-init-state []
  (let [x (c/shuffle-rubiks-cube (c/rubiks-cube-nxnxn 3))]
    {:shuffled-state x :current-state x :solution [] :last-move-applied -1}))

(declare rubiks-cube show-solution render-rubiks-cube)
(defn myaddnodes [nd children]
  (.addNodes nd children))
(def trf {:counter-clockwise {:z [[0 -1 0] [1 0 0] [0 0 1]]
                              :y [[0 0 1] [0 1 0] [-1 0 0]]
                              :x [[1 0 0] [0 0 -1] [0 1 0]]}
          :clockwise {:z [[0 1 0] [-1 0 0] [0 0 1]]
                      :y [[0 0 -1] [0 1 0] [1 0 0]]
                      :x [[1 0 0] [0 0 1] [0 -1 0]]}})

(let [clockwise-order {:x [:y :nz :ny :z]
                       :y [:x :z :nx :nz]
                       :z [:x :ny :nx :y]}]
  (defn rotate [{:keys [x y z nx ny nz] :as inp-props} dir ort]
    (let [t (mapv vec
                  (take 4 (partition 2 1 (let [t (clockwise-order dir)]
                                           (cycle (case ort :clockwise t :counter-clockwise (rseq t)))))))
          ret (reduce (fn [out-props [old new]] (assoc out-props new (inp-props old))) inp-props t)]
      ret)))

(defn spmult [mat pos cube-size]
  (let [ret (mapv (fn [row]
                    (let [p (map vector row pos)]
                      (first
                       (keep (fn [[xc x]]
                               (case xc
                                 1 x
                                 -1 (- cube-size 1 x)
                                 0 nil))
                             p)))) mat)]
    ret))

(defn instrument [x component-name]
  (let [x-meta (meta x)
        new-x-meta (into {} (map #(let [f (% x-meta)]
                                     [% (fn [& args]
                                          (println {% args :component-name component-name})
                                          (js/console.log (clj->js {% (clj->js args)}))
                                          (cond
                                           f (apply f args)
                                           (= % :should-component-update) true))])
                                  [:get-initial-state :component-will-receive-props
                                   :component-will-mount :should-component-update
                                   :component-did-mount :component-will-update
                                   :component-did-update :component-will-unmount]))]
    (with-meta x new-x-meta)))


(let [x (new-random-init-state)
      app-state (reagent/atom x)
      render-chan (chan)]
  (go (>! render-chan [:reset (:current-state x)]))
  (defn current-state-updater [move-id]
    (fn [& s]
      (let [{:keys [current-state last-move-applied solution] :as app-state-val} @app-state]
        (swap! app-state (fn [app-state-val]
                           (assoc app-state-val
                             :current-state (let [moves (cond
                                                         (< last-move-applied move-id) (subvec solution (inc last-move-applied) (inc move-id))
                                                         (> last-move-applied move-id) (mapv (fn [[mid [dir crd o]]] [mid [dir crd (s/opposite-orientation o)]])
                                                                                             (reverse (subvec solution (inc move-id) (inc last-move-applied))))
                                                         :default [])
                                                  new-current-state (c/apply-algorithm current-state (map second moves))]
                                              (go (onto-chan render-chan (map (fn [[_ op]] [:apply {:op op}]) moves) false))
                                              new-current-state)
                             :last-move-applied move-id))))))
  (defn solve-rubiks-cube []
    (let [{:keys [shuffled-state]} @app-state
          solution (s/solve-rubiks-cube shuffled-state)]
      (swap! app-state (fn [app-state-val] (assoc app-state-val :solution solution :last-move-applied -1)))))

  (defn shuffle-rubiks-cube []
    (let [x (new-random-init-state)]
      (swap! app-state (fn [& _] x))
      (go (>! render-chan [:reset (:current-state x)]))))

  (defn main-page []
    (let [{:keys [shuffled-state current-state orientation solution]} @app-state]
      [:div
       [rubiks-cube {:rubiks-cube-state current-state :orientation orientation :canvas-id "state-after-selected-move"}]
       [show-solution {:solution solution}]
       [:button {:on-click shuffle-rubiks-cube} "shuffle"]
       [:button {:on-click solve-rubiks-cube} "solve"]]))
  (go-loop [rcs nil]
           (let [[op val] (<! render-chan)]
             (case op
               :reset (do
                        (render-rubiks-cube "current-state" val)
                        (recur val))
               :apply (let [{:keys [n]} rcs
                            rotate-op-callback-fn-channel (chan)
                            cube-size n
                            scene (js/SceneJS.getScene)
                            {[dir coord orientation :as op] :op :keys [animation-duration num-steps] :or {animation-duration 1.0 num-steps 30}} val
                             interval (/ animation-duration num-steps)
                             get-coord (case dir :x #(.getX %) :y #(.getY %) :z #(.getZ %))
                             group-fn #(if (= (get-coord %) coord) :to-be-rotated :others)
                             new-rcs (go (c/apply-algorithm rcs [op]))
                             applying-op-done (chan)
                             cleanup-done (chan)
                             tick-rotater-chan (chan)
                             mysplit (fn [s sep]
                                       (.split s sep))
                             get-face-color (fn [n]
                                              [(keyword (.pop (mysplit (.-id n) "-"))) (.getColor n)])
                             set-face-color (fn [new-colors]
                                              (fn [nd-face]
                                                (let [dir (keyword (.pop (mysplit (.-id nd-face) "-")))]
                                                  (.setColor nd-face (new-colors dir)))))]
                        (println {:transform val})
                        (.getNode scene "rubiks-cube-pieces"
                                  (fn [node]
                                    (let [{:keys [to-be-rotated others]} (group-by group-fn (.disconnectNodes node))
                                          rotate-op-callback (fn [node]
                                                               (let [tick-rotater (.on scene "tick"
                                                                                       #(let [angle (.getAngle node)]
                                                                                          (if (>= (abs angle) 90)
                                                                                            (let [trf (fn [pos [dir _ ort]]
                                                                                                        (let [mat (-> trf ort dir)
                                                                                                              ret (spmult mat pos cube-size)]
                                                                                                          ret))
                                                                                                  trf-node (fn [nd]
                                                                                                             (let [colors (into {} (map get-face-color (.-nodes nd)))
                                                                                                                   pos-in [(.getX nd) (.getY nd) (.getZ nd)]
                                                                                                                   [x y z :as pos-out] (trf pos-in op)
                                                                                                                   [dir _ ort] op]
                                                                                                               (.setXYZ nd (clj->js {:x x :y y :z z}))
                                                                                                               (let [rotated-colors (rotate colors dir ort)
                                                                                                                     color-setter (set-face-color rotated-colors)]
                                                                                                                 (doseq [face-node (.-nodes nd)]
                                                                                                                   (color-setter face-node)))))]
                                                                                              (doseq [x to-be-rotated]
                                                                                                (trf-node x))
                                                                                              (doseq [id ["pre-rotate-translate-op" "rotate-op" "post-rotate-translate-op"]]
                                                                                                (.getNode scene id (fn [n] (.splice n))))
                                                                                              (close! applying-op-done))
                                                                                            (.setAngle node (+ angle (let [d 10] (case orientation :clockwise (- d) :counter-clockwise d)))))))]
                                                                 (go (>! tick-rotater-chan tick-rotater))))]
                                      (myaddnodes node (clj->js others))
                                      (.addNode node (clj->js (into (assoc {:x 1.5 :y 1.5 :z 1.5} dir 0)
                                                                    {:type "translate" :id "post-rotate-translate-op"
                                                                     :nodes [(into (assoc {:x 0 :y 0 :z 0} dir 1)
                                                                                   {:type "rotate" :id "rotate-op" :angle 0
                                                                                    :nodes [(into (assoc {:x -1.5 :y -1.5 :z -1.5} dir 0)
                                                                                                  {:type "translate" :id "pre-rotate-translate-op"})]})]})))
                                      (.getNode scene "pre-rotate-translate-op" #(myaddnodes % (clj->js to-be-rotated)))
                                      (.getNode scene "rotate-op" rotate-op-callback)
                                      (go (<! applying-op-done)
                                          (.off scene (<! tick-rotater-chan))
                                          (close! cleanup-done)))))
                        (let [nrcs (<! new-rcs)]
                          (<! cleanup-done)
                          (recur nrcs)))))))

(defn square-color [{:keys [color]}]
  [:span {:style {:margin "2px" :border "1px solid black" :width "30px" :height "30px" :display :inline-block :background-color color}}])

(defn rubiks-cube-face [{:keys [face]}]
  (into [:div] (map (fn [row] (into [:div] (map #(do [square-color {:color %}]) row))) face)))

(def layout [[nil :top nil nil] [:left :front :right :back] [nil :bottom nil nil]])
(let [rgb {:red {:r 1 :g 0 :b 0}
           :blue {:r 0 :g 0 :b 1}
           :green {:r 0 :g 1 :b 0}
           :white {:r 1 :g 1 :b 1}
           :orange {:r 1 :g 0.4 :b 0}
           :yellow {:r 1 :g 1 :b 0}
           :black {:r 0 :g 0 :b 0}}
      rgba (fn rgba [color]
             (let [{:keys [r g b]} (rgb color)]
               [r g b 1.0]))
      coords [0 1]
      cid (fn [x]
            (let [[i j k] (c/coord-map-to-coord-vec x)]
              (+ (* 2 (+ (* 2 i) j)) k)))
      positions (for [i coords j coords k coords] [i j k])
      piece-sg (fn [piece-id {:keys [faces position] :as piece}]
                 (into position {:type "translate"
                                 :id (str "rubiks-piece-" piece-id)
                                 :nodes (mapv (fn [{:keys [face color normal] :as f :or {color :black}}]
                                                (let [face-id (str "rubiks-piece-"  piece-id "-" (let [[dir val] (first normal)]
                                                                                                   (str (if (= val -1) "n" "") (name dir))))]
                                                  {:type "material" :color (rgb color)
                                                   :id face-id
                                                   :nodes (let [[v0 v1 v2 v3 :as vertices] (map cid face)]
                                                            [{:type "geometry"
                                                              :primitive "triangles"
                                                              :indices (js/Float32Array. (clj->js [v0 v1 v2 v0 v2 v3]))
                                                              :positions (js/Float32Array. (clj->js (apply concat positions)))}])})) faces)}))]
  (defn render-rubiks-cube [canvas-id {:keys [n] :as rcs}]
    (let [scene-id (str "scene-" canvas-id)
          scene (if-let [scene-x (js/SceneJS.getScene scene-id)]
                  (do
                    (.removeNodes scene-x)
                    scene-x)
                  (js/SceneJS.createScene (clj->js {:type "scene" :id scene-id :canvasId canvas-id})))
          camera-node (clj->js {:type "cameras/orbit"
                                :parent scene
                                :yaw 315
                                :pitch 30
                                :zoom 3.0
                                :zoomSensitivity 1.0
                                :eye {:x 2 :y 2 :z 2}
                                :look {:x 0 :y 0 :z 0}
                                :nodes [{:type "translate" :x -0.5 :y -0.5 :z -0.5
                                         :nodes [(let [s (/ 1.0 n)]
                                                   {:type "scale" :x s :y s :z s
                                                    :id "rubiks-cube-pieces"
                                                    :nodes (map-indexed piece-sg (c/rubiks-cube-geometry rcs))})]}]})]
      (.addNode scene camera-node))))



(defn rubiks-cube [{:keys [rubiks-cube-state orientation canvas-id]}]
  (let [{frcs :faces} (c/face-representation rubiks-cube-state)]
    [:div
     (into [:table {:style {:display :inline-block}}]
           (map (fn [table-row]
                  (into [:tr]
                        (mapv (fn [x] [:td (if x [rubiks-cube-face {:face (frcs x)}])])
                              table-row))) layout))]))

(defn show-solution [{:keys [solution orientation]}]
  (let [color :blue]
    (into [:div {:style {:background-color "#ccc" :padding "10px"}}
           [:h3  "click to see the state of the cube after applying all the transformations up-to and including clicked transformation" [:br]]]
          (map (fn [[move-id [dir coord orientation]]]
                 [:span {:key move-id
                         :title (if (< move-id 0) "start"
                                  (str move-id " [ " (name dir) " " coord " " (name orientation) " ]"))
                         :on-click (current-state-updater move-id)
                         :style {:margin-right "5px" :font-size "16pt" :width "3em" :height "1em"
                                 :margin-left "5px" :margin-top "3px" :margin-bottom "3px"
                                 :display :inline-block :border (str "10px solid " (name color)) :padding "2px"}}
                  (if (< move-id 0) "start"
                      (str (name dir) " " coord " " (if (= orientation :clockwise) "\u21BB" "\u21BA")))])
               (cons [-1 [nil nil nil]] solution)))))

(defn ^:export run []
  (js/SceneJS.setDebugConfigs (clj->js {:shading {:whitewash true :logScripts true}
                                        :webgl {:logTrace true}
                                        :pluginPath "js/scenejs/plugins"}))
  (reagent/render-component [main-page] (. js/document (getElementById "reactjs-content"))))
