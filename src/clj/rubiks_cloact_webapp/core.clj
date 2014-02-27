(ns rubiks-cloact-webapp.core
    (:require [compojure.handler :as handler]
              [compojure.route :as route]
              [compojure.core :refer [GET POST defroutes]]
              [clojure.walk :refer [walk]]
              [ring.util.response :as resp :refer [response]]
              [ring.middleware.json :refer [wrap-json-response
                                            wrap-json-body
                                            wrap-json-params]]))

(defn keywordize-all-strings [x]
  (walk (fn [iform] (cond
                    (string? iform) (keyword iform)
                    (coll? iform) (keywordize-all-strings iform)
                    :else iform)) identity x))

(defroutes app-routes
  (GET "/" [] (resp/redirect "/index.html"))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> #'app-routes
      handler/api
      wrap-json-body
      wrap-json-params
      wrap-json-response))
