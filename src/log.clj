(ns log
  (:require [clojure.string :as str]
            [rev-list :as rl]
            [file-io :as fio]
            [rev-parse :as rp]
            [clojure.java.io :as io]))

(defn first-pos [eval x]
  (loop [arr x n 0]
    (if (eval (first arr))
      n
      (recur (next arr) (inc n)))))

(defn format-oneline-output
  [address message]
  (str (subs address 0 7) " " message "\n"))

(defn get-commit-chain-log
  "provides a list of commit chain objects"
  [dir db head-address]
  (loop [info-list []
         target-address head-address]
    (let [lines (str/split (rl/commit-contents dir db target-address) #"\n")
          second-line (-> lines
                          second
                          (str/split #" "))
          message-line (->> lines
                            (map #(first (str/split % #" ")))
                            (first-pos (partial = "committer"))
                            (+ 2)
                            (nth lines))]
      (if (-> second-line first (= "parent"))
        (recur (conj info-list [target-address message-line]) (second second-line))
        (conj info-list [target-address message-line])))))

(defn switch-handler
  [number ref dir db]
  (let [ref (if (nil? ref)
              "HEAD"
              ref)]
    (cond
      (nil? number) (println "Error: you must specify a numeric count with '-n'.")
      (->> number (re-matches #"[0-9]+") nil?) (println "Error: the argument for '-n' must be a non-negative integer.")
      (or (= "@" ref) (= "HEAD" ref)) (switch-handler number (-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last) dir db)
      (not (.exists (io/file dir db "/refs/heads/" ref))) (->> ref (format "Error: could not find ref named %s.") println)
      :else (try (->> ref
                      (rl/ref-to-chain dir db get-commit-chain-log)
                      (take (Integer/parseInt number))
                      (map (fn [tuple] (format-oneline-output (first tuple) (second tuple))))
                      (reduce str "")
                      print)
                 (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e)))))

(defn log
  [args dir db]
  (let [[oneline ref & rest] args
        ref (if (nil? ref)
              "HEAD"
              ref)]
    (cond
      (fio/check-db-missing dir db) (println "Error: could not find database. (Did you run `idiot init`?)")
      (not (= oneline "--oneline")) (println "Error: log requires the --oneline switch")
      (= ref "-n") (switch-handler (first rest) (second rest) dir db)
      (or (= "@" ref) (= "HEAD" ref)) (log ["--oneline" (-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last)] dir db)
      (not (.exists (io/file dir db "/refs/heads/" ref))) (->> ref (format "Error: could not find ref named %s.") println)
      :else (try (->> ref
                      (rl/ref-to-chain dir db get-commit-chain-log)
                      (map (fn [tuple] (format-oneline-output (first tuple) (second tuple))))
                      (reduce str "")
                      print)
                 (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e)))))