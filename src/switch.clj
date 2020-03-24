(ns switch
  (:require [clojure.java.io :as io]
            [rev-parse :as rp]))

(defn refs-address
  [branch dir db]
  (str dir db "/refs/heads/" branch))

(defn head-address
  [dir db]
  (str dir db "/HEAD"))

(defn write-ref-to-head
  [branch dir db]
  (io/delete-file (head-address dir db))
  (spit (head-address dir db) (str "ref: " (str "refs/heads/" branch) "\n")))

(defn handle-c-switch
  [branch dir db]
  (if (.exists (io/as-file (refs-address branch dir db)))
    (println "Error: a ref with that name already exists.")
    (do (->> (str (rp/get-head-ref-address dir db) "\n")
             (spit (refs-address branch dir db)))
        (write-ref-to-head branch dir db)
        (println (str "Switched to a new branch '" branch "'")))))

(defn handle-no-switch
  [branch dir db]
  (if (not (.exists (io/as-file (refs-address branch dir db))))
    (println "Error: no ref with that name exists.")
    (do (write-ref-to-head branch dir db)
        (println (str "Switched to branch '" branch "'")))))

(defn switch
  [args dir db]
  (cond
    (or (= 0 (count args)) (and (= "-c" (first args)) (= 1 (count args)))) (println "Error: you must specify a branch name.")
    (or (< 2 (count args)) (and (not (= "-c" (first args))) (< 1 (count args)))) (println "Error: you may only specify one branch name.")
    (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (= "-c" (first args)) (handle-c-switch (second args) dir db)
    :else (handle-no-switch (first args) dir db)))