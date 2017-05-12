(ns wiring.secret
  (:require [buddy.core.codecs :as bc]
            [buddy.core.crypto :as b]
            [buddy.core.nonce :as bn]
            [clojure.tools.reader.edn :as edn]))

(defn generate-key []
  (bc/bytes->hex (bn/random-bytes 32)))

(def ^:private block-size
  (b/block-size (b/block-cipher :aes :cbc)))

(defn decrypt [cipher-text secret-key]
  (let [[iv cipher-bytes] (map byte-array (split-at block-size (bc/hex->bytes cipher-text)))]
    (edn/read-string (b/decrypt cipher-bytes (bc/hex->bytes secret-key) iv))))

(defn encrypt [plain-obj secret-key]
  (let [iv (bn/random-bytes block-size)]
    (str (bc/bytes->hex iv)
         (bc/bytes->hex (b/encrypt (bc/str->bytes (pr-str plain-obj)) (bc/hex->bytes secret-key) iv)))))

(comment
  (let [foo-key (generate-key)]
    (-> {:a 1 :b 2}
        (encrypt foo-key)
        (doto prn)
        (decrypt foo-key))))
