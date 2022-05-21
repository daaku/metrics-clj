(defproject org.clojars.daaku/metrics "1.1.0"
  :description "Clojure API over the Prometheus Java SDK."
  :url "https://github.com/daaku/metrics-clj"
  :scm {:name "git" :url "https://github.com/daaku/metrics-clj"}
  :license {:name "MIT License"}
  :dependencies [[io.prometheus/simpleclient "0.15.0"]
                 [io.prometheus/simpleclient_common "0.15.0" :scope "test"]
                 [org.clojure/clojure "1.11.1" :scope "provided"]])
