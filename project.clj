(defproject org.clojars.kriyative/cynojure "1.0.4"
  :description "Cynojure - clojure library"
  :url "http://github.com/kriyative/cynojure"
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [net.java.dev.jets3t/jets3t "0.7.3"]
                 [com.google.code.typica/typica "1.7"]]
  :dev-dependencies [[lein-clojars "0.5.0"]]
  :aot [cynojure.cl cynojure.util cynojure.sql cynojure.aws])
