(defproject clojure-pl "0.1.0"
  :description "pls"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.18"]
                 [acolfut "0.3.0"]]
  :plugins [[lein-colortest "0.3.0"]
            [lein-typed "0.3.5"]]
  :core.typed {:check [clojure-pl.lc.utlctc]}
  :aliases {"metainterp" ["run" "-m" "clojure-pl.lisp.metainterp.repl"]}
  :main ^:skip-aot clojure-pl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
