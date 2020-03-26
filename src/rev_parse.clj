(ns rev-parse
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn is-ref?
  [address]
  (-> address
      slurp
      (str/split #" ")
      first
      (= "ref:")))

(defn get-contents-no-nl
  [address]
  (reduce str (-> address
                  slurp
                  (str/split #" ")
                  second
                  butlast)))

(defn get-head-ref-address
  [dir db]
  (->> (str dir db "/HEAD")
       get-contents-no-nl
       (str dir db "/")
       io/file
       slurp
       butlast
       (reduce str)))

(defn rev-parse
  [args dir db]
  (let [branch (first args)
        db-address (str dir db "/")
        head-address (str db-address "HEAD")
        ref-address (str db-address "/refs/heads/" branch)]
    (cond
      (= 0 (count args)) (println "Error: you must specify a branch name.")
      (< 1 (count args)) (println "Error: you must specify a branch name and nothing else.")
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (or (= branch "HEAD") (= branch "@")) (if (is-ref? head-address)
                                                (println (get-head-ref-address dir db))
                                                (print (slurp head-address)))
      (not (.exists (io/as-file ref-address))) (println (str "Error: could not find ref named " branch ".") )
      :else (print (slurp ref-address)))))
