(ns wiring.core)

(defrecord Component [value stop!])

(defn ->component
  ([value] (->component value (fn [])))
  ([value stop!] (->Component value stop!)))

(defn start-system [config]
  (reduce (fn [system [k {:keys [wiring/component]}]]
            (let [started-component (component)]
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
