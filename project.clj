(defproject sir-model "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [proto-repl "0.3.1"]
                 [proto-repl-charts "0.3.2"]
                 [proto-repl-sayid "0.1.3"]
                 [anglican "1.0.0"]]
  :main ^:skip-aot sir-model.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
