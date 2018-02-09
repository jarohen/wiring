(ns wiring.core-test
  (:require [wiring.core :as sut]
            [wiring.secret :as secret]
            [clojure.test :as t]))

(t/deftest applies-switches
  (t/is (= (#'sut/apply-switches {:wiring/key :foo

                                  :config-key :value

                                  :switched? (sut/switch
                                               :the-switch true
                                               false)

                                  :overruled? (sut/switch
                                                :other-switch true
                                                :the-switch false)

                                  :component-too? (sut/switch
                                                    :component-switch true)

                                  :uh-oh? (sut/switch
                                            :not-this-one true)}

                                 #{:the-switch :other-switch :foo/component-switch})
           {:wiring/key :foo
            :config-key :value
            :switched? true
            :overruled? true
            :component-too? true
            :uh-oh? nil})))

(defn test-component [{:keys [!log k]}]
  (fn [config]
    (swap! !log conj [:start k config])
    (sut/->Component [:started k]
                     (fn []
                       (swap! !log conj [:stop k])))))

(t/deftest one-component-system
  (let [!log (atom [])
        system-map {:component {:wiring/component (test-component {:!log !log, :k :component})

                                :config-key :value
                                :switched? (sut/switch
                                             :the-switch true
                                             false)
                                :uh-oh? (sut/switch
                                          :not-this-one true)}}

        started-system (sut/start-system system-map {:switches #{:the-switch}})]

    (t/is (= @!log [[:start :component {:config-key :value, :switched? true, :uh-oh? nil}]]))

    (sut/stop-system started-system)

    (t/is (= @!log [[:start :component {:config-key :value, :switched? true, :uh-oh? nil}] [:stop :component]]))))

(t/deftest dep-specs
  (let [!log (atom [])
        started-system (sut/start-system {:wiring/deps #{:c1 :c2}
                                          :wiring/overrides {:c1 {:wiring/component (test-component {:!log !log, :k :c1})
                                                                  :c2 (sut/dep :c2)}

                                                             :c2 {:wiring/component (test-component {:!log !log, :k :c2})}}
                                          :wiring/switches #{}})]

    (t/is (= @!log [[:start :c2 {}] [:start :c1 {:c2 [:started :c2]}]]))

    (sut/stop-system started-system)

    (t/is (= @!log [[:start :c2 {}] [:start :c1 {:c2 [:started :c2]}] [:stop :c1] [:stop :c2]]))))

(t/deftest handles-component-fail
  (let [!log (atom [])
        started-system (try
                         (sut/start-system {:wiring/deps #{:c1 :c2}
                                            :wiring/overrides {:c1 {:wiring/component (fn [config]
                                                                                        (swap! !log conj [:start :c1 config])
                                                                                        (throw (ex-info "boom" {})))
                                                                    :wiring/deps [:c2]}

                                                               :c2 {:wiring/component (test-component {:!log !log, :k :c2})}}})
                         (throw (ex-info "shouldn't get here" {}))

                         (catch Exception e
                           (t/is (not= (.getMessage e) "shouldn't get here"))))]

    (t/is (= @!log [[:start :c2 {}] [:start :c1 {:c2 [:started :c2]}] [:stop :c2]]))))

(def my-component-config
  {:wiring/component (fn [config]
                       (select-keys config [:foo]))
   :foo :bar})

(t/deftest looks-up-component-config-sym
  (t/is (= :bar
           (-> (sut/start-system {:wiring/deps #{'wiring.core-test/my-component-config}})
               (get-in ['wiring.core-test/my-component-config :foo])))))

(defn mk-my-component [config]
  {:ok? true})

(t/deftest resolves-secrets
  (let [secret-key (secret/generate-key)]
    (t/is (= (-> (sut/start-system {:my-component {:wiring/component identity
                                                   :password (sut/->Secret :my-key (secret/encrypt "password123" secret-key))}}
                                   {:secret-keys {:my-key secret-key}})
                 (get-in [:my-component :password]))
             "password123"))))

(t/deftest parses-switches
  (t/is (= :db/live (#'sut/parse-switch "db/live")))
  (t/is (= :live (#'sut/parse-switch "live"))))

(comment
  (sut/with-system {:wiring/deps #{#'foo}
                    :wiring/overrides {#'foo {:wiring/component (fn [_]
                                                                  (sut/->Component ,,,))}}
                    :wiring/secret-keys {,,,}}
    (fn [_]
      ,,,))

  (def my-component
    {:wiring/component (fn [config]
                         (prn "starting component")
                         (sut/->Component [:component config]
                                          (fn []
                                            (prn "stopping component"))))
     :username "james"})

  (sut/defsystem api
    {:wiring/deps #{#'my-component}
     :wiring/secret-keys {}
     :wiring/overrides {,,,}})

  (start-api!)
  (stop-api!)
  @!api)
