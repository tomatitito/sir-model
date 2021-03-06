(defproject sir-model "0.1.0"
  :description "An implementation of a probabillistic version of the SIR-model."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [metasoarous/oz "1.3.1"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.cli "0.4.1"]
                 [anglican "1.0.0"]
                 [com.climate/claypoole "1.1.4"]
                 [javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
                 ]
  :main sir-model.core
  :aot [sir-model.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

