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

(t/deftest one-component-system
  (let [!log (atom [])
        component-config {:config-key :value}
        system-map {:component {:wiring/component (fn [config]
                                                    (swap! !log conj [:start-component config])
                                                    (sut/->component :started-component
                                                                     (fn []
                                                                       (swap! !log conj :stop-component))))

                                :wiring/switches {:the-switch {:switched? true}
                                                  :not-this-one {:uh-oh? true}}

                                :config-key :value
                                :switched? false}}

        started-system (sut/start-system system-map {:switches [:the-switch]})]

    (t/is (= @!log [[:start-component {:config-key :value, :switched? true}]]))

    (sut/stop-system started-system)

    (t/is (= @!log [[:start-component {:config-key :value, :switched? true}] :stop-component]))))
