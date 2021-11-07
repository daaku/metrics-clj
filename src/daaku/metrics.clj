(ns daaku.metrics
  "Clojure API over the Prometheus Java SDK.

  This API doesn't try to keep you far away from the Java API. You
  will likely end up using that API directly. The functions here
  provide idiomatic (and performant) access to the Collectors.

  All Collector creation functions take the same set of
  options. `:name` and `:help` are required, `:namespace`,
  `:subsystem` & `:labels` are optional."

  (:refer-clojure :exclude [inc dec set time])
  (:require [clojure.string :as str])
  (:import [io.prometheus.client
            Counter
            Counter$Child
            Collector
            CollectorRegistry
            Enumeration
            Enumeration$Child
            Gauge
            Gauge$Child
            Info
            Info$Child
            Histogram
            Histogram$Child
            Summary
            Summary$Child]))

(defn sanitize
  "Sanitize a value to create a safe string suitable as a name,
  namespace, subsystem or label."
  ^String [v]
  (-> ^String (if (keyword? v)
                (name v)
                (str v))
      (Collector/sanitizeMetricName)
      (str/replace #"__+" "_")
      (str/replace #"(^_+|_+$)" "")))

(defn register
  "Register into a CollectorRegistry the given Collector."
  [^CollectorRegistry r ^Collector c]
  (.register r c))

(defn- sanitize-strs ^"[Ljava.lang.String;" [vs]
  (into-array String (map sanitize vs)))

(defn- sanitize-info ^java.util.Map [info]
  (let [m (java.util.HashMap.)]
    (dorun (map (fn [[k v]]
                  (.put m (sanitize k) (if (keyword? v)
                                         (name v)
                                         (str v))))
                info))
    m))

(defprotocol WithLabels
  (with-labels
    [this labels]
    "Create a child with the provided labels."))

(defprotocol Increment
  (inc
    [this]
    [this amount]
    "Increment a Collector by 1 or the given amount."))

(defprotocol Decrement
  (dec
    [m]
    [m amount]
    "Decrement a Collector by 1 or the given amount."))

(defprotocol Settable
  (set
    [m amount]
    "Set a Collector's value to the given amount."))

(defprotocol Observe
  (observe
    [this amount]
    "Observe a given value.")

  (start-timer
    [this]
    "Start a timer. Returns a function that when called will stop the timer."))

(defmacro time
  "Measure in the Collector the run time duration of the given forms."
  [collector & body]
  `(let [stop# (start-timer ~collector)]
     (try
       (do ~@body)
       (finally
         (stop#)))))

(defprotocol State
  (set-state
    [this state]
    "Set the state of an Enumeration Collector. State value will be sanitized."))

(defprotocol SetInfo
  (set-info
    [this info]
    "Set the information of an Info Collector. Info keys and values will
     be sanitized."))

(defn- build-collector [^io.prometheus.client.SimpleCollector$Builder builder
                        {:keys [name help namespace subsystem labels]}]
  (.. builder
      (name (sanitize name))
      (help help))
  (when namespace
    (.namespace builder (sanitize namespace)))
  (when subsystem
    (.subsystem builder (sanitize subsystem)))
  (when labels
    (.labelNames builder (sanitize-strs labels)))
  (.create builder))

(defn counter
  "Create a Counter Collector."
  [opts]
  (build-collector (Counter/build) opts))

(extend-type Counter
  WithLabels
  (with-labels [^Counter this labels]
    (.labels this (sanitize-strs labels)))

  Increment
  (inc
    ([^Counter this]
     (.inc this))
    ([^Counter this amount]
     (.inc this (double amount)))))

(extend-type Counter$Child
  Increment
  (inc
    ([^Counter$Child this]
     (.inc this))
    ([^Counter$Child this amount]
     (.inc this (double amount)))))

(defn gauge
  "Create Gauge Collector."
  [opts]
  (build-collector (Gauge/build) opts))

(extend-type Gauge
  WithLabels
  (with-labels [^Gauge this labels]
    (.labels this (sanitize-strs labels)))

  Increment
  (inc
    ([^Gauge this]
     (.inc this))
    ([^Gauge this amount]
     (.inc this (double amount))))

  Decrement
  (dec
    ([^Gauge this]
     (.dec this))
    ([^Gauge this amount]
     (.dec this (double amount))))

  Settable
  (set [^Gauge this amount]
    (.set this (double amount))))

(extend-type Gauge$Child
  Increment
  (inc
    ([^Gauge$Child this]
     (.inc this))
    ([^Gauge$Child this amount]
     (.inc this (double amount))))

  Decrement
  (dec
    ([^Gauge$Child this]
     (.dec this))
    ([^Gauge$Child this amount]
     (.dec this (double amount))))

  Settable
  (set [^Gauge$Child this amount]
    (.set this (double amount))))

(defn summary
  "Create Summary Collector."
  [opts]
  (build-collector (Summary/build) opts))

(extend-type Summary
  WithLabels
  (with-labels [^Summary this labels]
    (.labels this (sanitize-strs labels)))

  Observe
  (observe [^Summary this amount]
    (.observe this (double amount)))

  (start-timer [^Summary this]
    (let [timer (.startTimer this)]
      #(.close timer))))

(extend-type Summary$Child
  Observe
  (observe [^Summary$Child this amount]
    (.observe this (double amount)))

  (start-timer [^Summary$Child this]
    (let [timer (.startTimer this)]
      #(.close timer))))

(defn histogram
  "Create Histogram Collector."
  [opts]
  (build-collector (Histogram/build) opts))

(extend-type Histogram
  WithLabels
  (with-labels [^Histogram this labels]
    (.labels this (sanitize-strs labels)))

  Observe
  (observe [^Histogram this amount]
    (.observe this (double amount)))

  (start-timer [^Histogram this]
    (let [timer (.startTimer this)]
      #(.close timer))))

(extend-type Histogram$Child
  Observe
  (observe [^Histogram$Child this amount]
    (.observe this (double amount)))

  (start-timer [^Histogram$Child this]
    (let [timer (.startTimer this)]
      #(.close timer))))

(defn enumeration
  "Create Enumeration Collector. Also requires `:states`."
  [{:keys [states] :as opts}]
  (build-collector (-> (Enumeration/build)
                       (.states (sanitize-strs states)))
                   opts))

(extend-type Enumeration
  WithLabels
  (with-labels [^Enumeration this labels]
    (.labels this (sanitize-strs labels)))

  State
  (set-state [^Enumeration this state]
    (.state this (sanitize state))))

(extend-type Enumeration$Child
  State
  (set-state [^Enumeration$Child this state]
    (.state this (sanitize state))))

(defn info
  "Create Info Collector."
  [opts]
  (build-collector (Info/build) opts))

(extend-type Info
  WithLabels
  (with-labels [^Info this labels]
    (.labels this (sanitize-strs labels)))

  SetInfo
  (set-info [^Info this info]
    (.info this (sanitize-info info))))

(extend-type Info$Child
  SetInfo
  (set-info [^Info$Child this info]
    (.info this (sanitize-info info))))
