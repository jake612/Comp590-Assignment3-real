(ns log
  (:require [clojure.string :as str]
            [rev-list :as rl]))

(defn get-commit-chain-contents
  "provides a list of commit chain objects"
  [dir db head-address]
  (loop [chain-list []
         target-address head-address]
    (let [contents (rl/commit-contents dir db target-address)
          second-line (-> contents
                          (str/split #"\n")
                          second
                          (str/split #" "))]
      (if (-> second-line first (= "parent"))
        (recur (conj chain-list contents) (second second-line))
        (conj chain-list contents)))))

(defn log
  [args dir db])