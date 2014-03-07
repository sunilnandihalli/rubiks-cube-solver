(ns rubiks-cloact-webapp.core
  (:require
   [rubiks-cloact-webapp.solver :as s]
   [rubiks-cloact-webapp.cube :as c]
   [reagent.core :as reagent :refer [atom]]))

;; Lets you do (prn "stuff") to the console
(enable-console-print!)
(def random-cube (c/shuffle-rubiks-cube (c/rubiks-cube-nxnxn 3)))
(defn new-random-init-state []
  (let [x random-cube]
    {:shuffled-state x :current-state x :solution []
     :last-move-applied -1}))

(declare rubiks-cube show-solution)

(let [app-state (reagent/atom (new-random-init-state))]
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
                                              new-current-state)
                             :last-move-applied move-id))))))

  (defn solve-rubiks-cube []
    (let [{:keys [shuffled-state]} @app-state
          solution (s/solve-rubiks-cube shuffled-state)]
      (swap! app-state (fn [app-state-val] (assoc app-state-val :solution solution :last-move-applied -1)))))

  (defn shuffle-rubiks-cube []
    (swap! app-state (fn [& _] (new-random-init-state))))

  (defn main-page []
    (let [{:keys [shuffled-state current-state orientation solution]} @app-state]
      [:div
       [rubiks-cube {:rubiks-cube-state shuffled-state :orientation  orientation :canvas-id "shuffled-state"}]
       [rubiks-cube {:rubiks-cube-state current-state :orientation orientation :canvas-id "state-after-selected-move"}]
       [show-solution {:solution solution}]
       [:button {:on-click shuffle-rubiks-cube} "shuffle"]
       [:button {:on-click solve-rubiks-cube} "solve"]])))

(defn square-color [{:keys [color]}]
  [:span {:style {:margin "2px" :border "1px solid black" :width "30px" :height "30px" :display :inline-block :background-color color}}])

(defn rubiks-cube-face [{:keys [face]}]
  (into [:div] (map (fn [row] (into [:div] (map #(do [square-color {:color %}]) row))) face)))

(def layout [[nil :top nil nil] [:left :front :right :back] [nil :bottom nil nil]])

(defn rubiks-cube [{:keys [rubiks-cube-state orientation canvas-id]}]
  (let [{frcs :faces} (c/face-representation rubiks-cube-state)]
    [:div
     (into [:table {:style {:display :inline-block}}]
           (map (fn [table-row]
                  (into [:tr]
                        (mapv (fn [x] [:td (if x [rubiks-cube-face {:face (frcs x)}])])
                              table-row))) layout))
     [:canvas {:id canvas-id :width 600 :height 400}]]))

(defn show-solution [{:keys [solution orientation]}]
  (let [color :blue]
    (into [:div {:style {:background-color "#ccc" :padding "10px"}}
           [:h3  "click to see the state of the cube after applying all the transformations up-to and including clicked transformation" [:br]]]
          (map (fn [[move-id [dir coord orientation]]]
                 [:span {:key move-id
                         :title (str move-id " [ " (name dir) " " coord " " (name orientation) " ]")
                         :on-click (current-state-updater move-id)
                         :style {:margin-right "5px" :font-size "16pt" :width "3em" :height "1em" :margin-left "5px" :margin-top "3px" :margin-bottom "3px"
                                 :display :inline-block :border (str "10px solid " (name color)) :padding "2px"}} (str (name dir) " " coord " "
                                                                                                                   (if (= orientation :clockwise) "\u21BB" "\u21BA"))]) solution))))
(def rgb {:red {:r 1 :g 0 :b 0}
          :blue {:r 0 :g 0 :b 1}
          :green {:r 0 :g 1 :b 0}
          :white {:r 1 :g 1 :b 1}
          :orange {:r 1 :g 0.4 :b 0}
          :yellow {:r 1 :g 1 :b 0}
          :black {:r 0 :g 0 :b 0}})
(defn rgba [color]
  (let [{:keys [r g b]} (rgb color)]
    [r g b 1.0]))
(comment (def misc-nodes [{:type "prims/teapot" :id (str "teapot-" canvas-id)}
                  {:type "prims/box" :id (str "box-" canvas-id)}
                  {:type "prims/sphere" :id (str "sphere-" canvas-id)}
                  {:type "objects/plants/ghostTree" :id (str "gt-" canvas-id)}]))
(let [coords [0 1]
      cid (fn [x]
            (let [[i j k] (c/coord-map-to-coord-vec x)]
              (+ (* 2 (+ (* 2 i) j)) k)))
      positions (for [i coords j coords k coords] [i j k])
      piece-sg (fn [{:keys [faces position] :as piece}]
                 (into position {:type "translate"
                                 :nodes (mapv (fn [{:keys [face color] :as f :or {color :black}}]
                                                {:type "material" :color (rgb color)
                                                 :nodes (let [[v0 v1 v2 v3 :as vertices] (map cid face)]
                                                          [{:type "geometry"
                                                            :primitive "triangles"
                                                            :indices (js/Float32Array. (clj->js [v0 v1 v2 v0 v2 v3]))
                                                            :positions (js/Float32Array. (clj->js (apply concat positions)))}])}) faces)}))]
  (defn render-rubiks-cube [canvas-id rcs]
    (js/SceneJS.createScene
     (clj->js {:type "scene" :id (str "scene-" canvas-id) :canvasId canvas-id
               :nodes [{:type "lookAt" :eye {:x 5 :y 5 :z 5}
                        :nodes [{:type "scale" :x 1.0 :y 1.0 :z 1.0 :id (str "tscale-" canvas-id)
                                 :nodes (mapv piece-sg (c/rubiks-cube-geometry rcs))}]}]})))
  (defn render-rubiks-cube [canvas-id {:keys [n] :as rcs}]
    (let [s (/ 1.0 n)]
     (js/SceneJS.createScene
      (clj->js {:type "scene" :id (str "scene-" canvas-id) :canvasId canvas-id
                :nodes [{:type "cameras/orbit"
                         :yaw 1
                         :pitch 1
                         :zoom 3.0
                         :zoomSensitivity 1.0
                         :eye {:x 2 :y 2 :z 2}
                         :look {:x 0 :y 0 :z 0}
                         :nodes [{:type "translate" :x -0.5 :y -0.5 :z -0.5
                                  :nodes [{:type "scale" :x s :y s :z s
                                           :nodes (mapv piece-sg (c/rubiks-cube-geometry rcs))}]}]}]}))))

  (defn ^:export run []
    (js/SceneJS.setDebugConfigs (clj->js {:shading {:whitewash true :logScripts true}
                                          :webgl {:logTrace true}
                                          :pluginPath "js/scenejs/plugins"}))
    (reagent/render-component [main-page] (.-body js/document))
    (let [shuffled-state random-cube
          current-state shuffled-state]
      (mapv render-rubiks-cube ["shuffled-state"  "state-after-selected-move"] [shuffled-state current-state] ))))
