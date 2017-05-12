(ns wiring.core
  (:require [com.stuartsierra.dependency :as d]))

(defn noop [])

(defrecord Component [value stop!])

(defn ->component
  ([value] (->component value noop))
  ([value stop!] (->Component value stop!)))

(defn apply-switches [config {:keys [switches]}]
  (apply merge (dissoc config :wiring/switches)
         (for [switch switches]
           (get-in config [:wiring/switches switch]))))

(defn normalise-deps [deps]
  (cond
    (map? deps) deps
    (vector? deps) (->> deps
                        (mapv (fn [dep]
                                (cond
                                  (map? dep) dep
                                  (keyword? dep) {dep dep})))
                        (apply merge))))

(defn order-components [config]
  (->> config
       (reduce (fn [graph [k {:keys [wiring/deps]}]]
                 (reduce (fn [graph dep]
                           (d/depend graph k (val dep)))
                         (d/depend graph ::system k)
                         deps))
               (d/graph))
       d/topo-sort
       butlast))

(defn start-component [{component-fn :wiring/component, :keys [wiring/deps] :as component-config} {:keys [components switches]}]
  (component-fn (-> component-config
                    (apply-switches {:switches switches})
                    (merge (into {}
                                 (map (fn [[k dep]]
                                        [k (:value (get components dep))]))
                                 deps))

                    (dissoc :wiring/component :wiring/switches :wiring/deps))))

(defn stop-system [{:keys [components component-order] :as system}]
  (doseq [k (reverse component-order)]
    (let [{:keys [stop!]} (get components k)]
      (try
        (stop!)
        (catch Exception e
          ;; TODO log
          )))))

(defn start-system [config {:keys [switches]}]
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
                                                                                          :switches switches})}
                                           (catch Exception e
                                             {:error e}))]
           (if component
             (recur more-ks
                    (conj started-components k)
                    (-> components
                        (assoc k (cond-> component
                                   (not (instance? Component component)) ->component))))

             (do
               (stop-system {:components components, :component-order started-components})

               (throw (ex-info "Error starting system" {:error error
                                                        :component k})))))))}))
