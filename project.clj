(defproject hello "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [commons-validator/commons-validator "1.7"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :target-path "target/%s"
  :profiles {:repl
             {:dependencies [[org.clojure/tools.namespace "0.2.10"]]
              :repl-options {:init-ns hello.core}
              :injections [(require 'clojure.tools.namespace.repl)
                           (require '[clojure.tools.namespace.repl :refer [refresh]])]}})
