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
                 [org.clojure/clojurescript "1.7.228" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.async "0.2.374" :exclusions [org.clojure/tools.reader]]
                 [cljs-http "0.1.39" :exclusions [commons-codec org.clojure/tools.reader]]
                 [reagent "0.5.1"]
                 [prismatic/dommy "1.1.0"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-ring "0.9.7"]
            [lein-pdo "0.1.1"]
          ;  [lein-gorilla "0.3.6"]
            ]
  :hooks [leiningen.cljsbuild]
  :aot :all

  :aliases {"dev" ["pdo" "cljsbuild" "auto" "dev," "ring" "server-headless"]}

  :ring {:handler rubiks-cloact-webapp.core/app
         :init    rubiks-cloact-webapp.core/init}

  :source-paths ["src/clj" "src/cljc"]

  :cljsbuild {:builds
              {:dev
               {:id "dev"
                :source-paths ["src/cljs" "src/cljc"]
                :compiler {:output-to "resources/public/js/rubiks_cloact_webapp.js"
                           :output-dir "resources/public/js/out/"
                           :optimizations :whitespace
                           :verbose true
                           :warnings true
                           :closure-warnings {:access-controls :warning
                                              :ambiguous-function-decl :warning
                                              :debugger-statement-present :warning
                                              :check-regexp :warning
                                              :check-types :warning
                                              :check-useless-code :warning
                                              :check-variables :warning
                                              :const :warning
                                              :constant-property :warning
                                              :deprecated :warning
                                              :duplicate-message :warning
                                              :es5-strict :warning
                                              :externs-validation :warning
                                              :fileoverview-jsdoc :warning
                                              :global-this :warning
                                              :internet-explorer-checks :warning
                                              :invalid-casts :warning
                                              :missing-properties :warning
                                              :non-standard-jsdoc :warning
                                              :strict-module-dep-check :warning
                                              :tweaks :warning
                                              :undefined-names :warning
                                              :undefined-variables :warning
                                              :unknown-defines :warning
                                              :visiblity :warning}

                           :externs ["js/scenejs/scenejs.js"]}}}})
