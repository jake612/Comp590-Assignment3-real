(ns branch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-master-name
  [dir db]
  (-> (str dir db "/HEAD")
      slurp
      (str/split #"\n")
      first
      (str/split #"/")
      last))

(defn handle-delete
  [branch dir db]
  (cond
    (not (->> branch
              (str dir db "/refs/heads/")
              io/as-file
              .exists)) (println (str "Error: branch '" branch "' not found."))
    (= branch (get-master-name dir db)) (println (str "Error: cannot delete checked-out branch '" branch "'."))
    :else (do (io/delete-file (str dir db "/refs/heads/" branch))
              (println (str "Deleted branch " branch ".")))))

(defn print-branches
  [dir db]
  (let [names (->> (str dir db "/refs/heads")
                   io/file
                   .listFiles
                   (sort-by #(.getName %)))
        master (get-master-name dir db)]
    (doseq [branch names] (let [name (.getName branch)]
                            (if (= name master)
                              (println (str "* " name))
                              (println (str "  " name)))))))

(defn branch
  [args dir db]
  (let [[switch branch] args]
    (cond
      (and (= switch "-d") (= 1 (count args))) (println "Error: you must specify a branch name.")
      (or (and (not (= switch "-d")) (not (nil? switch))) (< 2 (count args))) (println "Error: invalid arguments.")
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= switch "-d") (handle-delete branch dir db)
      :else (print-branches dir db))))