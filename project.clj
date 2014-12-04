(defproject sableman "0.1.0-SNAPSHOT"
  :description "The Sable bundle manager."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.xml "0.0.7"]
                 [org.clojure/tools.cli "0.3.1"]
                 [slingshot "0.10.3"]
                 [hiccup "1.0.5"]
                 [enlive "1.1.5"]
                 [fleet "0.10.1"]
                 [me.raynes/fs "1.4.6"]
                 [commons-io "2.4"]
                 [digest "1.4.4"]]
  :main ^:skip-aot sableman.core
  ;:target-path "target/%s"
  :target-path "target/"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-bin "0.3.4"]]
  :bin {:name "sab"})
