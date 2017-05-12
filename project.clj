(defproject jarohen/wiring "0.1.0-SNAPSHOT"
  :description "A Clojure library to configure and wire-up component-based applications"
  :url "https://github.com/jarohen/wiring"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/tools.reader "1.0.0-beta4"]

                 [camel-snake-kebab "0.4.0"]
                 [com.stuartsierra/dependency "0.2.0"]
                 [buddy/buddy-core "1.2.0"]])
