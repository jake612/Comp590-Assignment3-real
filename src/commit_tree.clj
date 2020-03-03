(ns commit-tree
  (:require [clojure.java.io :as io]
            [git]
            [write-wtree :as wt]
            [file-io :as fio]))

(def author_committer "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500")

(defn file-path
  [file-name dir db]
  (str dir db "/objects/" (subs file-name 0 2) "/" (subs file-name 2)))

(defn get-object-type
  [address dir db]
  (->> (file-path address dir db)
       fio/unzip
       (fio/split-at-byte (byte 0x20))
       first
       fio/bytes->str))

(defn commit-object
  [commits-str author-str tree-addr message]
  (let [commit-format (str "tree %s\n"
                           "%s"
                           "author %s\n"
                           "committer %s\n"
                           "\n"
                           "%s\n")
        commit-str (format commit-format
                           tree-addr
                           commits-str
                           author-str
                           author-str
                           message)]
    (format "commit %d\000%s"
            (count commit-str)
            commit-str)))

(defn which-true
  "given a function for testing and a sequence, returns the first value that evaluates to false"
  [func seq]
  (try (let [evaluation (take-while func seq)]
         (if (= (count seq) (count evaluation))
           nil
           (->> evaluation count (nth seq))))
       (catch Exception e nil)))

(defn parent-commit-handler
  "function takes care of the case where there is a p-switch"
  ([message tree-addr dir db] (parent-commit-handler message tree-addr [] dir db))
  ([message tree-addr parent-commits dir db]
   (let [exists-eval (which-true #(.exists (io/as-file (file-path % dir db))) parent-commits)
         type-eval (which-true #(= (get-object-type % dir db) "commit") parent-commits)
         commits-concat (fn [x] (reduce str "" (map #(str "parent " % "\n") x)))]
     (cond
       (not (nil? exists-eval)) (println (format "Error: no commit object exists at address %s." exists-eval))
       (not (nil? type-eval)) (println (format "Error: an object exists at address %s, but it isn't a commit." type-eval))
       :else (-> (commits-concat parent-commits)
                 (commit-object author_committer tree-addr message)
                 .getBytes
                 (wt/write-object db)
                 println)))))

(defn commit-tree
  "function for handling commit-tree"
  [args dir db]
  (let [[tree-addr m-switch message p-switch & parent-commits] args]
    (cond
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? tree-addr) (println "Error: you must specify a tree address.")
      (not (.exists (io/as-file (file-path tree-addr dir db)))) (println "Error: no tree object exists at that address.")
      (not= (get-object-type tree-addr dir db) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
      (not= m-switch "-m") (println "Error: you must specify a message.")
      (nil? message) (println "Error: you must specify a message with the -m switch.")
      (= p-switch "-p") (cond
                          (nil? parent-commits) (println "Error: you must specify a commit object with the -p switch")
                          :else (parent-commit-handler message tree-addr parent-commits dir db))
      :else (parent-commit-handler message tree-addr dir db))))



