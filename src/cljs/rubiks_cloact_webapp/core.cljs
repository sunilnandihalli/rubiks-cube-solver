(ns rubiks-cloact-webapp.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require
   [goog.net.XhrIo :as xhr]
   [chord.client :refer [ws-ch]]
   [reagent.core :as reagent :refer [atom]]
   [goog.events :as events]
   [cljs.core :as c]
   [cljs.core.async :refer [put! <! >! chan timeout]]
   [cljs-http.client :as http]))

;; Lets you do (prn "stuff") to the console
(enable-console-print!)
(def ^:dynamic *editable* nil)
(def app-state (atom {:rubiks-cube-state (into {} (map (fn [[s c]] [s (vec (repeat 3 (vec (repeat 3 c))))])
                                                       {:front :white :back :yellow :top :red :bottom :orange :right :green :left :blue}))
                      :solution []}))

(defn choices [rcs path]
  [:not-set :red :blue :green :yellow :white :orange])

(defn square-color-choice [a [rcs path]]
  (let [c (get-in @rcs path)]
    (if-not *editable* [:span {:style {:margin "2px" :border "1px solid black" :width "10px" :height "10px" :display :inline-block :background-color (if (= c :not-set) :black c)}}]
      (let [pc (choices @rcs path)]
        (into [:select {:value c
                        :on-change (fn [s]
                                     (js/console.log s))}] (mapv #(do [:option {:value %} (name %)]) pc))))))

(defn rubiks-cube-face [a [rcs path]]
  (into [:div] (map (fn [row] (into [:div] (map #(do [square-color-choice rcs (into path [row %])]) (range 3)))) (range 3))))

(def layout [[nil :top nil nil] [:left :front :right :back] [nil :bottom nil nil]])

(defn rubiks-cube [a [rcs path]]
  (into [:table {:style {:display :inline-block}}]
        (map (fn [table-row]
               (into [:tr]
                     (mapv (fn [x] [:td (if x [rubiks-cube-face rcs (conj path x)])])
                           table-row))) layout)))

(defn apply-algorithm [rcs moves]
  (go
   (let [req-data {:json-params {:rubiks-cube-state rcs :moves moves}}
         response (<! (http/post "/apply_transform" req-data))]
     (swap! app-state assoc :current-state (-> response :body :transformed-rcs)))))

(defn show-solution [a [app-state path]]
  (into [:div {:style {:background-color "#ccc" :padding "10px"}}
         [:h3  "click to see the state of the cube after applying all the transformations up-to and including clicked transformation" [:br]]]
        (map (fn [[move-id [color orientation]]]
               [:span {:title (str (name color) " " (name orientation))
                       :on-click (fn [& s]
                                   (apply-algorithm (@app-state :rubiks-cube-state) (take (inc move-id) (:solution @app-state))))
                       :style {:margin-right "5px" :font-size "12pt" :width "1em" :height "1em" :margin-left "5px" :margin-top "3px" :margin-bottom "3px" :display :inline-block :border (str "10px solid " (name color)) :padding "2px"}} (if (= orientation :clockwise) "\u21BB" "\u21BA")])
             (map vector (range) (get-in @app-state path)))))

(defn shuffle-rubiks-cube []
  (go
   (let [response (<! (http/get "/shuffled_rcs"))
         rcs (into {} (map (fn [[k v]] [k (mapv #(mapv keyword %) v)])
                           (seq (-> response :body :random-rcs))))]
     (swap! app-state assoc :rubiks-cube-state rcs :solution []))))

(defn random-rubiks-cube [])
(defn solve-rubiks-cube []
  (go
   (let [req-data {:json-params (select-keys @app-state [:rubiks-cube-state])}
         response (<! (http/post "/solve_rcs" req-data))
         moves (->> response :body :moves (mapv #(mapv keyword %)))]
     (swap! app-state assoc :solution moves))))
(defn show-editable-rubiks-cube [])
(defn main-page [a [app-state path]]
  [:div
   [rubiks-cube app-state (into path [:rubiks-cube-state])]
   [rubiks-cube app-state (into path [:current-state])]
   [show-solution app-state (into path [:solution])]
   [:button {:on-click shuffle-rubiks-cube} "shuffle"]
   ;[:button {:on-click random-rubiks-cube} "random rubiks cube"]
   [:button {:on-click solve-rubiks-cube} "show solution"]
   ;[:button {:on-click show-editable-rubiks-cube} "enter my own rubiks configuration"]
   ])

(defn ^:export run []
  (reagent/render-component [main-page app-state []] (.-body js/document)))
