(ns wiring.core)

(defn noop [])

(defrecord Component [value stop!])

(defn ->component
  ([value] (->component value noop))
  ([value stop!] (->Component value stop!)))

(defn apply-switches [config {:keys [switches]}]
  (apply merge (dissoc config :wiring/switches)
         (for [switch switches]
           (get-in config [:wiring/switches switch]))))

(defn start-system [config {:keys [switches]}]
  (reduce (fn [system [k {component-fn :wiring/component, :as component-config}]]
            (let [started-component (component-fn (-> component-config
                                                      (apply-switches {:switches switches})
                                                      (dissoc :wiring/component :wiring/switches)))]
              (-> system
                  (assoc-in [:components k] (cond-> started-component
                                              (not (instance? Component started-component)) ->component))
                  (update :component-order conj k))))

          {:component-order []}
          config))

(defn stop-system [{:keys [components component-order] :as system}]
  (doseq [k (reverse component-order)]
    (let [{:keys [stop!]} (get components k)]
      (stop!))))
