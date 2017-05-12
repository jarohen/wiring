(ns wiring.core
  (:require [wiring.secret :as secret]
            [com.stuartsierra.dependency :as d]
            [clojure.walk :as w]))

(defn- noop [])

(defrecord Component [value stop!])

(defrecord Secret [key-id cipher-text])

(defn- apply-switches [config {:keys [switches]}]
  (apply merge (dissoc config :wiring/switches)
         (for [switch switches]
           (get-in config [:wiring/switches switch]))))

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

(defn- resolve-component-fn [component-fn]
  (if (symbol? component-fn)
    (if-let [sym-ns (some-> (namespace component-fn) symbol)]
      (or (do
            (require sym-ns)
            (ns-resolve (find-ns sym-ns) (symbol (name component-fn))))

          (throw (ex-info "Can't find :wiring/component sym" {:sym component-fn})))

      (throw (ex-info ":wiring/component symbols must be fully qualified" {:sym component-fn})))

    component-fn))

(defn- start-component [{:keys [wiring/deps] :as component-config} {:keys [components switches secret-keys]}]
  (let [component-fn (resolve-component-fn (:wiring/component component-config))]
    (component-fn (-> component-config
                      (apply-switches {:switches switches})
                      (merge (into {}
                                   (map (fn [[k dep]]
                                          [k (:value (get components dep))]))
                                   deps))

                      (->> (w/postwalk (fn [v]
                                         (if (instance? Secret v)
                                           (let [{:keys [key-id cipher-text]} v]
                                             (if-let [secret-key (get secret-keys key-id)]
                                               (secret/decrypt cipher-text secret-key)
                                               (throw (ex-info "missing secret-key" {:key-id key-id}))))

                                           v))))

                      (dissoc :wiring/component :wiring/switches :wiring/deps)))))

(defn stop-system [{:keys [components component-order] :as system}]
  (doseq [k (reverse component-order)]
    (let [{:keys [stop!]} (get components k)]
      (try
        (stop!)
        (catch Exception e
          ;; TODO log
          )))))

(defn start-system [config {:keys [switches secret-keys]}]
  (let [config (->> config
                    (into {}
                          (map (fn [[k component]]
                                 [k (update component :wiring/deps normalise-deps)]))))

        component-order (order-components config)]

    {:component-order component-order

     :components
     (loop [[k & more-ks] component-order
            started-components []
            components {}]
       (if-not k
         components

         (let [component-config (get config k)
               {:keys [component error]} (try
                                           {:component (start-component component-config {:components components
                                                                                          :switches switches
                                                                                          :secret-keys secret-keys})}
                                           (catch Exception e
                                             {:error e}))]
           (if component
             (recur more-ks
                    (conj started-components k)
                    (-> components
                        (assoc k (cond-> component
                                   (not (instance? Component component)) (-> (->Component noop))))))

             (do
               (stop-system {:components components, :component-order started-components})

               (throw (ex-info "Error starting system" {:component k} error)))))))}))
