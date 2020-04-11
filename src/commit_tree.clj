(ns commit-tree
  (:require [git]
            [write-wtree :as wt]
            [file-io :as fio]
            [get_address :as ga]))

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
           [(->> evaluation count (nth seq)) (count evaluation)]))
       (catch Exception e
         e
         nil)))

(defn parent-commit-handler
  "function takes care of the case where there is a p-switch"
  [message tree-addr parent-commits dir db]
  (let [commit-pairs (partition-all 2 parent-commits)
        address-info (map #(ga/search-address (second %) dir db) commit-pairs)
        com-length (second (first (which-true #(> (count (second %)) 3) commit-pairs)))
        matches (which-true #(= 1 (first %)) address-info)
        type-eval (which-true #(= (get-object-type (second %) dir db) "commit") address-info)
        commits-concat (fn [x] (reduce str "" (map #(str "parent " % "\n") x)))]
    (cond
      (= (count (last commit-pairs)) 1) (println "Error: you must specify a commit object with the -p switch.")
      (not (nil? com-length)) (println (format "Error: too few characters specified for address '%s'" com-length))
      (not (nil? matches)) (ga/addr-loc-error-handler (->> (second matches) (nth commit-pairs) second) (first (first matches)) (format "Error: no commit object exists at address %s." (->> (second matches) (nth commit-pairs) second)))
      (not (nil? type-eval)) (println (format "Error: an object exists at address %s, but it isn't a commit." (->> (second type-eval) (nth commit-pairs) second)))
      :else (-> (commits-concat (map second address-info))
                (commit-object author_committer tree-addr message)
                .getBytes
                (wt/write-object dir db)))))

(defn commit-tree
  "function for handling commit-tree"
  [args dir db]
  (let [[tree-addr m-switch message & parent-commits] args
        info (ga/search-address tree-addr dir db)
        address (second info)
        matching-addresses (first info)]
    (cond
      (fio/check-db-missing dir db) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? tree-addr) (println "Error: you must specify a tree address.")
      (< (count tree-addr) 4) (println (format "Error: too few characters specified for address '%s'" tree-addr))
      (not (= 1 matching-addresses)) (ga/addr-loc-error-handler tree-addr matching-addresses "Error: no tree object exists at that address.")
      (not= (get-object-type address dir db) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
      (not= m-switch "-m") (println "Error: you must specify a message.")
      (nil? message) (println "Error: you must specify a message with the -m switch.")
      :else (let [address (parent-commit-handler message address parent-commits dir db)]
              (when (not (nil? address))
                (println address))))))



