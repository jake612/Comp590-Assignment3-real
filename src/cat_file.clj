(ns cat-file
  (:require [clojure.java.io :as io]
            [file-io :as fio]
            [clojure.string :as str]
            [commit-tree :as ct]))

(defn cat-file
  "function for handling cat-file command"
  [args dir db]
  (let [switch (first args)
        address (second args)
        get-path #(str dir db "/objects/" (subs % 0 2) "/" (subs % 2))
        splitter #(str/split % #"\000")]
    (cond
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not= switch "-p") (not= switch "-t")) (println "Error: the -p or -t switch is required")
      (nil? address) (println "Error: you must specify an address")
      (not (.exists (io/as-file (get-path address)))) (println "Error: that address doesn't exist")
      :else (case switch
              "-p" (if (= (ct/get-object-type address dir db) "tree")
                     (->> address
                          get-path
                          fio/unzip
                          (fio/split-at-byte (byte 0x00))
                          count
                          println)
                     (->> address
                          get-path
                          fio/unzip
                          (map char)
                          (apply str)
                          splitter
                          second
                          print))
              "-t" (-> address
                       (ct/get-object-type dir db)
                       println)))))
