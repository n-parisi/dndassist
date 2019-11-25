(defproject dndassist "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.6"]
                 [net.sourceforge.htmlunit/htmlunit "2.36.0"]
                 [org.jsoup/jsoup "1.12.1"]]
  :main ^:skip-aot dndassist.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
