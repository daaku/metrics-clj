(ns daaku.metrics-test
  (:require [clojure.test :refer [deftest is]]
            [daaku.metrics :as m])
  (:import [io.prometheus.client CollectorRegistry]))

(def m-name "the_name")
(def m-help "the help message")
(def m-ns "the_ns")
(def m-subsystem "the_subsystem")
(def m-labels ["l_a" "l_b"])
(def m-fullname (str m-ns "_" m-subsystem "_" m-name))
(def opts {:name m-name
           :help m-help
           :namespace m-ns
           :subsystem m-subsystem})
(def opts-l (assoc opts :labels m-labels))
(def opts-states (assoc opts :states [:foo]))
(def opts-l-states (assoc opts-l :states [:foo]))

(deftest sanitize-test
  (dorun
   (map (fn [[in out]]
          (is (= (m/sanitize in) out)))
        [[:foo "foo"]
         [:foo-bar "foo_bar"]
         [:foo-bar--baz "foo_bar_baz"]
         ["foo-bar" "foo_bar"]
         ["foo" "foo"]])))

(deftest register-test
  (let [r (CollectorRegistry.)]
    (m/register r (m/counter opts))
    (is (= (.name (first (enumeration-seq (.metricFamilySamples r))))
           m-fullname))))

(deftest inc-test
  (dorun
   (map (fn [m]
          (is (= (.get m) (double 0)))
          (m/inc m 2)
          (is (= (.get m) (double 2)))
          (m/inc m)
          (is (= (.get m) (double 3))))
        [(m/counter opts)
         (m/with-labels (m/counter opts-l) m-labels)
         (m/gauge opts)
         (m/with-labels (m/gauge opts-l) m-labels)])))

(deftest dec-test
  (dorun
   (map (fn [m]
          (is (= (.get m) (double 0)))
          (m/dec m 2)
          (is (= (.get m) (double -2)))
          (m/dec m)
          (is (= (.get m) (double -3))))
        [(m/gauge opts)
         (m/with-labels (m/gauge opts-l) m-labels)])))

(deftest set-test
  (dorun
   (map (fn [m]
          (is (= (.get m) (double 0)))
          (m/set m 2)
          (is (= (.get m) (double 2)))
          (m/set m 42)
          (is (= (.get m) (double 42))))
        [(m/gauge opts)
         (m/with-labels (m/gauge opts-l) m-labels)])))

(deftest observe-test
  (dorun
   (map (fn [m]
          (m/observe m 2)
          ((m/start-timer m))
          (m/time m 42)
          ; this test has no real assertions
          (is (= 42 42)))
        [(m/histogram opts)
         (m/with-labels (m/histogram opts-l) m-labels)
         (m/summary opts)
         (m/with-labels (m/summary opts-l) m-labels)])))

(deftest enumeration-test
  (dorun
   (map (fn [m]
          (m/set-state m :foo)
          (is (= (.get m) "foo")))
        [(m/enumeration opts-states)
         (m/with-labels (m/enumeration opts-l-states) m-labels)])))

(deftest info-test
  (dorun
   (map (fn [m]
          (m/set-info m {:foo :bar
                         :answer 42})
          (is (= (.get m) {"foo" "bar"
                           "answer" "42"})))
        [(m/info opts)
         (m/with-labels (m/info opts-l) m-labels)])))