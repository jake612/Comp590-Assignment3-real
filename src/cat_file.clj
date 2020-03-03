(ns cat-file
  (:require [clojure.java.io :as io]
            [file-io :as fio]
            [clojure.string :as str]
            [commit-tree :as ct]))

(defn tree-cat-handler
  [file-path]
  (let [bytes-array (->> file-path
                         fio/unzip
                         (fio/split-at-byte 0x00))]
    ))

(defn cat-file
  "function for handling cat-file command"
  [args dir db]
  (let [switch (first args)
        address (second args)
        get-path #(str db "/objects/" (subs % 0 2) "/" (subs % 2))]
    (cond
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not= switch "-p") (not= switch "-t")) (println "Error: either the -p or the -t switch is required.")
      (nil? address) (println "Error: you must specify an address")
      (not (.exists (io/as-file (get-path address)))) (println "Error: that address doesn't exist")
      :else (case switch
              "-p" (print (second (str/split (fio/open-file (get-path address)) #"\000")))
              "-t" (-> address
                       (ct/get-object-type dir db)
                       println)))))
