(ns rubiks-cloact-webapp.core
  (:require
   [rubiks-cloact-webapp.solver :as s]
   [reagent.core :as reagent :refer [atom]]))

;; Lets you do (prn "stuff") to the console
(enable-console-print!)


(defn new-random-init-state []
  (let [x (s/scrambled-rubiks-cube)]
    {:shuffled-state x :current-state x :solution []
     :last-move-applied -1 :orientation (s/full-orientation {:front :green :right :red})}))

(def app-state (atom (new-random-init-state)))

(defn current-state-updater [move-id]
  (fn [&s]
    (swap! app-state (fn [{:keys [current-state last-move-applied solution] :as app-state-val}]
                       (assoc app-state-val
                         :current-state (let [moves (cond
                                                     (< last-move-applied move-id) (subvec solution (inc last-move-applied) (inc move-id))
                                                     (> last-move-applied move-id) (mapv (fn [[mid [c o]]] [mid [c (opposite-orientation o)]])
                                                                                         (rseq (subvec solution (inc move-id) (inc last-move-applied))))
                                                     :default [])]
                                          (s/apply-algorithm current-state moves))
                         :last-move-applied move-id)))))

(defn solve-rubiks-cube []
  (let [{:keys [shuffled-state]} @app-state
        solution (s/solve shuffled-state)]
    (swap! app-state (fn [app-state-val] (assoc app-state-val :solution solution :last-move-applied -1)))))

(defn shuffle-rubiks-cube []
  (js/console.log "shuffle-rubiks-cube called")
  (swap! app-state (fn [& _] (new-random-init-state)))
  (js/console.log @app-state))

(defn choices [rcs path]
  [:not-set :red :blue :green :yellow :white :orange])

(defn square-color [a [c]]
  [:span {:style {:margin "2px" :border "1px solid black" :width "30px" :height "30px" :display :inline-block :background-color c}}])

(defn rubiks-cube-face [a [f]]
  (into [:div] (map (fn [row] (into [:div] (map #(do [square-color %]) row))) f)))

(def layout [[nil :top nil nil] [:left :front :right :back] [nil :bottom nil nil]])

(defn rubiks-cube [a [frcs id]]
  [:div
   (into [:table {:style {:display :inline-block}}]
         (map (fn [table-row]
                (into [:tr]
                      (mapv (fn [x] [:td (if x [rubiks-cube-face (frcs x)])])
                            table-row))) layout))
   [:canvas {:id id :width 600 :height 400}]])

(defn show-solution [a [app-state path]]
  (into [:div {:style {:background-color "#ccc" :padding "10px"}}
         [:h3  "click to see the state of the cube after applying all the transformations up-to and including clicked transformation" [:br]]]
        (map (fn [[move-id [color orientation] :as x]]
               [:span {:title (str move-id " " (name color) " " (name orientation))
                       :on-click (current-state-updater move-id)
                       :style {:margin-right "5px" :font-size "12pt" :width "1em" :height "1em" :margin-left "5px" :margin-top "3px" :margin-bottom "3px"
                               :display :inline-block :border (str "10px solid " (name color)) :padding "2px"}} (if (= orientation :clockwise) "\u21BB" "\u21BA")])
             (:solution @app-state))))



(defn main-page [a [app-state path]]
  [:div
   [rubiks-cube app-state (into path [:shuffled-state]) "shuffled-state"]
   [rubiks-cube app-state (into path [:current-state]) "state-after-selected-move"]
   [show-solution app-state (into path [:solution])]
   [:button {:on-click shuffle-rubiks-cube} "shuffle"]
   [:button {:on-click solve-rubiks-cube} "solve"]])
(defn dr
  ([x] (js/console.log (clj->js x)) x)
  ([txt x] (js/console.log txt) (dr x)))
(defn render-teapot [canvas-id]
  (js/console.log "")
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
  (reagent/render-component [main-page app-state []] (.-body js/document))
  (mapv render-teapot ["shuffled-state"  "state-after-selected-move"]))
