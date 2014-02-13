(ns rubiks-cloact-webapp.server-solve
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require
   [rubiks-cloact-webapp.solver :as s]
   [goog.net.XhrIo :as xhr]
   [chord.client :refer [ws-ch]]
   [goog.events :as events]
   [cljs.core :as c]
   [cljs.core.async :refer [put! <! >! chan timeout]]
   [cljs-http.client :as http]))
(comment
 (defn apply-algorithm [rcs moves]
   (go
    (let [req-data {:json-params {:rubiks-cube-state rcs :moves moves}}
          response (<! (http/post "/apply_transform" req-data))]
      (swap! app-state assoc :current-state (-> response :body :transformed-rcs)))))

 (defn shuffle-rubiks-cube []
   (go
    (let [response (<! (http/get "/shuffled_rcs"))
          rcs (into {} (map (fn [[k v]] [k (mapv #(mapv keyword %) v)])
                            (seq (-> response :body :random-rcs))))]
      (swap! app-state assoc :rubiks-cube-state rcs :solution []))))

 (defn solve-rubiks-cube []
   (go
    (let [req-data {:json-params (select-keys @app-state [:rubiks-cube-state])}
          response (<! (http/post "/solve_rcs" req-data))
          moves (->> response :body :moves (mapv #(mapv keyword %)))]
      (swap! app-state assoc :solution moves)))))
