(ns commit
  (:require [clojure.java.io :as io]
            [commit-tree :as ct]
            [rev-parse :as rp]
            [branch :as bh]))

(defn handle-commit
  [commit-address dir db]
  (let [head-branch (bh/get-master-name dir db)
        ref-address (str dir db "/refs/heads/" head-branch)
        spit-to-ref #(spit ref-address (str % "\n"))]
    (when (not (nil? commit-address))
      (println "Commit created.")
      (if (rp/is-ref? (str dir db "/HEAD"))
        (do (if (.exists (io/as-file ref-address))
              (do (io/delete-file ref-address)
                  (spit-to-ref commit-address))
              (spit-to-ref commit-address))
            (println (str "Updated branch " head-branch ".")))
        (spit (str dir db "/HEAD") (str commit-address "\n"))))))

(defn commit
  "function for handling commit"
  [args dir db]
  (let [[tree-addr m-switch message & parent-commits] args]
    (cond
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? tree-addr) (println "Error: you must specify a tree address.")
      (not (.exists (io/as-file (ct/file-path tree-addr dir db)))) (println "Error: no tree object exists at that address.")
      (not= (ct/get-object-type tree-addr dir db) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
      (not= m-switch "-m") (println "Error: you must specify a message.")
      (nil? message) (println "Error: you must specify a message with the -m switch.")
      :else (handle-commit (ct/parent-commit-handler message tree-addr parent-commits dir db) dir db))))
