(ns wiring.core-test
  (:require [wiring.core :as sut]
            [clojure.test :as t]))

(t/deftest one-component-system
  (let [!log (atom [])
        component-config {:config-key :value}
        system-map {:component (merge {:wiring/component (fn [config]
                                                           (swap! !log conj [:start-component config])
                                                           (sut/->component :started-component
                                                                            (fn []
                                                                              (swap! !log conj :stop-component))))}
                                      component-config)}

        started-system (sut/start-system system-map)]

    (t/is (= @!log [[:start-component component-config]]))

    (sut/stop-system started-system)

    (t/is (= @!log [[:start-component component-config] :stop-component]))

    ))
