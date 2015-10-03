(defproject clojure-pl "0.1.0"
  :description "pls"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [acolfut "0.1.0"]]
  :plugins [[colortest "0.1.0"]]
  :main ^:skip-aot clojure-pl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
