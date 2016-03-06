(defproject rubiks-cloact-webapp "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.reader "0.10.0"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/core.logic "0.8.10"]
                 ;; CLJ
                 [ring/ring-core "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [compojure "1.4.0"]
                 [cheshire "5.5.0"]
                 ;; CLJS
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.2.374"]
                 [cljs-http "0.1.39"]
                 [reagent "0.5.1"]
                 [prismatic/dommy "1.1.0"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-ring "0.9.7"]
            [lein-pdo "0.1.1"]
            [lein-gorilla "0.3.6"]]



  :aliases {"dev" ["pdo" "cljsbuild" "auto" "dev," "ring" "server-headless"]}

  :ring {:handler rubiks-cloact-webapp.core/app
         :init    rubiks-cloact-webapp.core/init}

  :source-paths ["src/clj" "src/cljc"]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs" "src/cljc"]
                        :compiler {:output-to "resources/public/js/rubiks_cloact_webapp.js"
                                   :output-dir "resources/public/js/out"
                                   :optimizations :none
                                   :source-map true
                                   :externs ["js/scenejs/scenejs.js"]}}]})
