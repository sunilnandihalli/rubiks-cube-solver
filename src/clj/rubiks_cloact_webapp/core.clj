(ns rubiks-cloact-webapp.core
    (:require [compojure.handler :as handler]
              [compojure.route :as route]
              [compojure.core :refer [GET POST defroutes]]
              [chord.http-kit :refer [with-channel]]
              [clojure.core.async :refer [<! >! put! close! go go-loop]]
              [clojure.walk :refer [walk]]
              [ring.util.response :as resp :refer [response]]
              [ring.middleware.json :refer [wrap-json-response
                                            wrap-json-body
                                            wrap-json-params]]
              [cheshire.core :as json]
              [clojure.java.io :as io]
              [rubiks-cloact-webapp.solver :as s]))

(defn keywordize-all-strings [x]
  (walk (fn [iform] (cond
                    (string? iform) (keyword iform)
                    (coll? iform) (keywordize-all-strings iform)
                    :else iform)) identity x))



(defroutes app-routes
  (GET "/" [] (resp/redirect "/index.html"))

  (GET "/test" [] (response {:message "You made it!"}))
  (GET "/shuffled_rcs" [] (response {:random-rcs (s/to-faces (s/scrambled-rubiks-cube))}))
  (POST "/solve_rcs" {{:keys [rubiks-cube-state]} :params}
        (let [frcs (into {}
                         (map (fn [[k v]]
                                [k (mapv #(mapv keyword %) v)])
                              rubiks-cube-state))]
          (response {:moves (-> frcs s/from-faces s/solve meta :moves-applied)})))
  (POST "/apply_transform" {:keys [json-params]}
        (let [{krcs :rubiks-cube-state kmoves :moves} (keywordize-all-strings json-params)
              rcs (s/from-faces krcs)
              trcs (s/apply-algorithm rcs kmoves)
              ormp (into {} (map (fn [[k v]] [k (get-in v [1 1])]) krcs))]
          (response {:transformed-rcs (s/to-faces trcs ormp)})))
  (POST "/test" req (response {:message "Doing something something important..."}))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> #'app-routes
      handler/api
      wrap-json-body
      wrap-json-params
      wrap-json-response))
