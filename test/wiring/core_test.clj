(ns wiring.core-test
  (:require [wiring.core :as sut]
            [clojure.test :as t]))

(t/deftest one-component-system
  (let [!log (atom [])
        started-system (sut/start-system {:component {:wiring/component (fn []
                                                                          (swap! !log conj :start-component)
                                                                          (sut/->component :started-component
                                                                                           (fn []
                                                                                             (swap! !log conj :stop-component))))}})]

    (t/is (= @!log [:start-component]))

    (sut/stop-system started-system)

    (t/is (= @!log [:start-component :stop-component]))

    ))
