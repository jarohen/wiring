(ns wiring.core-test
  (:require [wiring.core :as sut]
            [clojure.test :as t]))

(t/deftest applies-switches
  (t/is (= (sut/apply-switches {:wiring/switches {:the-switch {:switched? true
                                                               :overruled? false}
                                                  :other-switch {:overruled? true}
                                                  :not-this-one {:uh-oh? true}}

                                :config-key :value
                                :switched? false}

                               {:switches [:the-switch :other-switch]})
           {:config-key :value
            :switched? true
            :overruled? true})))

(defn test-component [{:keys [!log k]}]
  (fn [config]
    (swap! !log conj [:start k config])
    (sut/->component [:started k]
                     (fn []
                       (swap! !log conj [:stop k])))))

(t/deftest one-component-system
  (let [!log (atom [])
        system-map {:component {:wiring/component (test-component {:!log !log, :k :component})

                                :wiring/switches {:the-switch {:switched? true}
                                                  :not-this-one {:uh-oh? true}}

                                :config-key :value
                                :switched? false}}

        started-system (sut/start-system system-map {:switches [:the-switch]})]

    (t/is (= @!log [[:start :component {:config-key :value, :switched? true}]]))

    (sut/stop-system started-system)

    (t/is (= @!log [[:start :component {:config-key :value, :switched? true}] [:stop :component]]))))

(t/deftest dep-specs
  (let [!log (atom [])
        system-map {:c1 {:wiring/component (test-component {:!log !log, :k :c1})
                         :wiring/deps [:c2]}

                    :c2 {:wiring/component (test-component {:!log !log, :k :c2})}}

        started-system (sut/start-system system-map {:switches []})]

    (t/is (= @!log [[:start :c2 {}] [:start :c1 {:c2 [:started :c2]}]]))

    (sut/stop-system started-system)

    (t/is (= @!log [[:start :c2 {}] [:start :c1 {:c2 [:started :c2]}] [:stop :c1] [:stop :c2]]))))

(t/deftest handles-component-fail
  (let [!log (atom [])
        system-map {:c1 {:wiring/component (fn [config]
                                             (swap! !log conj [:start :c1 config])
                                             (throw (ex-info "boom" {})))
                         :wiring/deps [:c2]}

                    :c2 {:wiring/component (test-component {:!log !log, :k :c2})}}

        started-system (try
                         (sut/start-system system-map {:switches []})
                         (throw (ex-info "shouldn't get here" {}))

                         (catch Exception e
                           (t/is (not= (.getMessage e) "shouldn't get here"))))]

    (t/is (= @!log [[:start :c2 {}] [:start :c1 {:c2 [:started :c2]}] [:stop :c2]]))))
