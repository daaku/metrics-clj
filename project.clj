(defproject org.clojars.daaku/metrics "1.0.0"
  :description "Clojure API over the Prometheus Java SDK."
  :url "https://github.com/daaku/metrics-clj"
  :scm {:name "git" :url "https://github.com/daaku/metrics-clj"}
  :license {:name "MIT License"}
  :dependencies [[io.prometheus/simpleclient "0.12.0"]
                 [org.clojure/clojure "1.10.3" :scope "provided"]])
