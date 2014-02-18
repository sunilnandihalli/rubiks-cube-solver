(ns rubiks-cloact-webapp.core
  (:require
   [rubiks-cloact-webapp.solver :as s]
   [reagent.core :as reagent :refer [atom]]))

;; Lets you do (prn "stuff") to the console
(enable-console-print!)


(defn new-random-init-state []
  (let [x (s/scrambled-rubiks-cube)]
    {:shuffled-state x :current-state x :solution []
     :last-move-applied -1
     :orientation (s/full-orientation {:front :green :right :red})}))

(declare rubiks-cube show-solution)

(def app-state (reagent/atom (new-random-init-state)))

(defn current-state-updater [move-id]
  (fn [& s]
    (println "calling current-state-updater ")
    (js/console.log s)
    (println move-id)
    (let [{:keys [current-state last-move-applied solution] :as app-state-val} @app-state]
      (swap! app-state (fn [app-state-val]
                         (assoc app-state-val
                           :current-state (let [moves (cond
                                                       (< last-move-applied move-id) (subvec solution (inc last-move-applied) (inc move-id))
                                                       (> last-move-applied move-id) (mapv (fn [[mid [c o]]] [mid [c (s/opposite-orientation o)]])
                                                                                           (reverse (subvec solution (inc move-id) (inc last-move-applied))))
                                                       :default [])
                                                new-current-state (s/apply-algorithm current-state (map second moves))]
                                            new-current-state)
                           :last-move-applied move-id))))))

(defn solve-rubiks-cube []
  (let [{:keys [shuffled-state]} @app-state
        solution (vec (map-indexed vector (s/solve shuffled-state)))]
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
     [:button {:on-click solve-rubiks-cube} "solve"]]))

(defn square-color [{:keys [color]}]
  [:span {:style {:margin "2px" :border "1px solid black" :width "30px" :height "30px" :display :inline-block :background-color color}}])

(defn rubiks-cube-face [{:keys [face]}]
  (into [:div] (map (fn [row] (into [:div] (map #(do [square-color {:color %}]) row))) face)))

(def layout [[nil :top nil nil] [:left :front :right :back] [nil :bottom nil nil]])

(defn rubiks-cube [{:keys [rubiks-cube-state orientation canvas-id]}]
  (let [frcs (s/to-faces rubiks-cube-state orientation)]
    [:div
     (into [:table {:style {:display :inline-block}}]
           (map (fn [table-row]
                  (into [:tr]
                        (mapv (fn [x] [:td (if x [rubiks-cube-face {:face (frcs x)}])])
                              table-row))) layout))
     [:canvas {:id canvas-id :width 600 :height 400}]]))


(defn show-solution [{:keys [solution]}]
  (into [:div {:style {:background-color "#ccc" :padding "10px"}}
         [:h3  "click to see the state of the cube after applying all the transformations up-to and including clicked transformation" [:br]]]
        (map (fn [[move-id [color orientation]]]
               [:span {:key move-id
                       :title (str move-id " " (name color) " " (name orientation))
                       :on-click (current-state-updater move-id)
                       :style {:margin-right "5px" :font-size "12pt" :width "1em" :height "1em" :margin-left "5px" :margin-top "3px" :margin-bottom "3px"
                               :display :inline-block :border (str "10px solid " (name color)) :padding "2px"}} (if (= orientation :clockwise) "\u21BB" "\u21BA")]) solution)))



(defn dr
  ([x] (js/console.log (clj->js x)) x)
  ([txt x] (js/console.log txt) (dr x)))

(defn render-teapot [canvas-id]
  (js/SceneJS.createScene
   (clj->js {:type "scene" :id (str "scene-" canvas-id) :canvasId canvas-id
             :nodes [{:type "lookAt" :eye {:x 5 :y 5 :z 5}
                      :nodes [{:type "scale" :x 1.0 :y 1.0 :z 1.0 :id (str "tscale-" canvas-id)
                               :nodes [{:type "material" :color {:r 0.3 :g 0.3 :b 1.0}
                                        :nodes [{:type "rotate" :id (str "rotate-" canvas-id) :y 1.0 :angle 0
                                                 :nodes [{:type "prims/teapot" :id (str "teapot-" canvas-id)}]}]}]}]}]})))

(defn render-rubiks-cube [canvas-id rcs])

(defn ^:export run []
  (js/SceneJS.setDebugConfigs (clj->js {:shading {:whitewash true :logScripts true}
                                        :webgl {:logTrace true}
                                        :pluginPath "js/scenejs/plugins"}))
  (reagent/render-component [main-page] (.-body js/document))
  (mapv render-teapot ["shuffled-state"  "state-after-selected-move"]))
