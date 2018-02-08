(ns wiring.core
  (:require [wiring.secret :as secret]
            [com.stuartsierra.dependency :as d]
            [clojure.set :as set]
            [clojure.string :as s]
            [clojure.walk :as w]))

(defrecord Component [value stop!])

(defrecord Secret [key-id cipher-text])

(defmacro switch
  "Takes a set of switch/expr clauses, and an optional default value.
  Returns the configuration from the first active switch, or the default if none are active, or nil.

  (w/switch
    <switch> <expr>
    <switch-2> <expr-2>
    ...
    <default-expr>)"
  {:style/indent 0}
  [& clauses]

  `(-> (fn [switches#]
         (condp #(contains? %2 %1) switches#
           ~@clauses
           ~@(when (zero? (mod (count clauses) 2))
               [nil])))
       (with-meta {::switch? true})))

(defn- apply-switches [{k :wiring/key, :as config} switches]
  (let [switches (set/union switches
                            (when k
                              (into #{}
                                    (keep (fn [switch]
                                            (when (= (name k) (namespace switch))
                                              (keyword (name switch)))))
                                    switches)))]
    (w/postwalk (fn [v]
                  (if (::switch? (meta v))
                    (v switches)
                    v))
                config)))

(defn- apply-secrets [config secret-keys]
  (w/postwalk (fn [v]
                (if (instance? Secret v)
                  (let [{:keys [key-id cipher-text]} v]
                    (if-let [secret-key (get secret-keys key-id)]
                      (secret/decrypt cipher-text secret-key)
                      (throw (ex-info "missing secret-key" {:key-id key-id}))))

                  v))
              config))

(defn- normalise-deps [deps]
  (cond
    (map? deps) deps
    (vector? deps) (->> deps
                        (mapv (fn [dep]
                                (cond
                                  (map? dep) dep
                                  (keyword? dep) {dep dep})))
                        (apply merge))))

(defn- order-components [config]
  (->> config
       (reduce (fn [graph [k {:keys [wiring/deps]}]]
                 (reduce (fn [graph dep]
                           (d/depend graph k (val dep)))
                         (d/depend graph ::system k)
                         deps))
               (d/graph))
       d/topo-sort
       butlast))

(defn- maybe-resolve-sym [maybe-sym]
  (if (symbol? maybe-sym)
    (if-let [sym-ns (some-> (namespace maybe-sym) symbol)]
      (or (do
            (require sym-ns)
            (some-> (ns-resolve (find-ns sym-ns) (symbol (name maybe-sym)))
                    deref))

          (throw (ex-info "Can't find symbol" {:sym maybe-sym})))

      (throw (ex-info "Wiring symbols must be fully qualified" {:sym maybe-sym})))

    maybe-sym))



(defn- start-component [{:keys [wiring/key wiring/deps] :as component-config} {:keys [system switches secret-keys]}]
  (let [resolved-config (-> component-config
                            (apply-switches switches)
                            (apply-secrets secret-keys)
                            (merge (into {}
                                         (map (fn [[k dep]]
                                                [k (get system dep)]))
                                         deps))

                            (dissoc :wiring/component :wiring/switches :wiring/deps :wiring/key))]

    (if-let [component-fn (:wiring/component component-config)]
      (component-fn resolved-config)
      resolved-config)))

(defn stop-system [system]
  (doseq [stop! (:stop-fns (meta system))]
    (try
      (stop!)
      (catch Exception e
        ;; TODO log
        ))))

(defn start-system [config {:keys [switches secret-keys]}]
  (let [config (->> config
                    (into {}
                          (map (fn [[k component]]
                                 [k (-> component
                                        maybe-resolve-sym
                                        (assoc :wiring/key k)
                                        (update :wiring/deps normalise-deps))]))))

        component-order (order-components config)]

    (loop [[k & more-ks] component-order
           started-components []
           system {}]
      (if-not k
        system

        (let [component-config (get config k)
              {:keys [component error]} (try
                                          {:component (start-component component-config {:system system
                                                                                         :switches switches
                                                                                         :secret-keys secret-keys})}
                                          (catch Exception e
                                            {:error e}))]
          (if-let [{:keys [value stop!]} (when component
                                           (if (instance? Component component)
                                             component
                                             (map->Component {:value component})))]
            (recur more-ks
                   (conj started-components k)
                   (-> system
                       (assoc k value)
                       (cond-> stop! (vary-meta update :stop-fns conj stop!))))

            (do
              (stop-system system)

              (throw (ex-info "Error starting system" {:component k} error)))))))))

(defn start! [!system config {:keys [switches secret-keys] :as opts}]
  (when-not (compare-and-set! !system nil ::starting)
    (throw (ex-info "System already starting/started" {})))

  (try
    (reset! !system (start-system config opts))

    (catch Exception e
      (reset! !system nil)
      (throw e))))

(defn stop! [!system]
  (let [system @!system]
    (when (and (map? system)
               (compare-and-set! !system system ::stopping))
      (try
        (stop-system system)
        (finally
          (reset! !system nil))))))



(defn- parse-switch [switch]
  (if-let [[_ switch-ns switch-name] (re-matches #"(.+?)/(.+)" switch)]
    (keyword switch-ns switch-name)
    (keyword switch)))

(def env-switches
  (-> (System/getenv "WIRING_SWITCHES")
      (some-> (s/split #","))
      (->> (into [] (map parse-switch)))))

(defmacro defsystem [name config]
  (let [atom-sym (symbol (format "!%s-system" name))]
    `(do
       (defonce ~atom-sym
         (atom nil))

       (defn ~(symbol (format "start-%s!" name)) []
         (let [{switches# :wiring/switches, secret-keys# :wiring/secret-keys :or {switches# env-switches}, :as config#} ~config]
           (start! ~atom-sym
                   (dissoc ~config :wiring/switches :wiring/secret-keys)
                   {:switches switches#, :secret-keys secret-keys#})))

       (defn ~(symbol (format "stop-%s!" name)) []
         (stop! ~atom-sym))

       ~atom-sym)))
