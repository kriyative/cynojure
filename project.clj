(defproject org.clojars.kriyative/cynojure "1.2.5"
  :description "Cynojure - clojure library"
  :url "http://github.com/kriyative/cynojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [net.java.dev.jets3t/jets3t "0.7.3"]
                 [com.google.code.typica/typica "1.7"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [lein-clojars "0.5.0"]
                     [lein-difftest "1.3.1"]]
  :hooks [leiningen.hooks.difftest]
  :test-path "test")
