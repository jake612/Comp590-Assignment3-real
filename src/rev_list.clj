(ns rev-list
  (:require [file-io :as fio]
            [rev-parse :as rp]
            [cat-file :as cf]
            [clojure.string :as str]))

(defn object-address
  [dir db address]
  (str dir db "/objects/" (subs address 0 2) "/" (subs address 2)))

(defn commit-contents
  [dir db address]
  (->> address
       (object-address dir db)
       cf/get-content-bytes
       (map char)
       (apply str)))


(defn get-commit-chain-addresses
  "provides a list of commit chain objects"
  [dir db head-address]
  (loop [chain-list []
         target-address head-address]
    (let [contents (commit-contents dir db target-address)
          second-line (-> contents
                          (str/split #"\n")
                          second
                          (str/split #" "))]
      (if (-> second-line first (= "parent"))
        (recur (conj chain-list target-address) (second second-line))
        (conj chain-list target-address)))))

(defn ref-to-chain
  [dir db get-function ref]
  (->> ref
       (str dir db "/refs/heads/")
       slurp
       butlast
       (reduce str)
       (get-function dir db)))

(defn switch-handler
  [number ref dir db]
  (cond
    (nil? number) (println "Error: you must specify a numeric count with '-n'")
    (->> number (re-matches #"[0-9]+") nil?) (println "Error: the argument for '-n' must be a non-negative integer")
    (or (= "@" ref) (= "HEAD" ref)) (switch-handler number (-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last) dir db)
    :else (try (->> ref
                    (ref-to-chain dir db get-commit-chain-addresses)
                    (take (Integer/parseInt number))
                    (map #(str % "\n"))
                    (reduce str "")
                    print)
               (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e))))

(defn rev-list
  [args dir db]
  (let [[ref & rest] args]
    (cond
      (fio/check-db-missing dir db) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= ref "-n") (switch-handler (first rest) (second rest) dir db)
      (or (= "@" ref) (= "HEAD" ref)) (rev-list [(-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last)] dir db)
      :else (try (->> ref
                      (ref-to-chain dir db get-commit-chain-addresses)
                      (map #(str % "\n"))
                      (reduce str "")
                      print)
                 (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e)))))
