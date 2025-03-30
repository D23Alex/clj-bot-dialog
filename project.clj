(defproject clj-bot-dialog "0.1.0-SNAPSHOT"
  :description "an eDSL for writing telegram bots"
  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [environ             "1.1.0"]
                 [morse               "0.4.3"]
                 [codax "1.4.0"]]

  :plugins [[lein-environ "1.1.0"]]

  :main ^:skip-aot clj-bot-dialog.core
  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})
