(defproject rubiks-cloact-webapp "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.3"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/core.logic "0.8.5"]
                 [jarohen/chord "0.2.2"]
                 ;; CLJ
                 [ring/ring-core "1.2.0"]
                 [ring/ring-json "0.2.0"]
                 [compojure "1.1.6"]
                 [cheshire "5.2.0"]
                 ;; CLJS
                 [org.clojure/clojurescript "0.0-2156"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [cljs-http "0.1.3"]
                 [reagent "0.3.0"]]

  :plugins [[lein-cljsbuild "1.0.1"]
            [lein-ring "0.8.7"]
            [lein-pdo "0.1.1"]
            [lein-gorilla "0.1.2"]
            [com.keminglabs/cljx "0.3.2"]]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/clj"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]}

  :hooks [cljx.hooks]
  :aliases {"dev" ["pdo" "cljsbuild" "auto" "dev," "ring" "server-headless"]}

  :ring {:handler rubiks-cloact-webapp.core/app
         :init    rubiks-cloact-webapp.core/init}

  :source-paths ["src/clj" "target/generated/clj"]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs" "target/generated/cljs"]
                        :compiler {:output-to "resources/public/js/rubiks_cloact_webapp.js"
                                   :output-dir "resources/public/js/out"
                                   :optimizations :none
                                   :source-map true
                                   :externs ["js/scenejs/scenejs.js"]}}]})
