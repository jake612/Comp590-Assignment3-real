(ns cat-file
  (:require [clojure.java.io :as io]
            [file-io :as fio]
            [clojure.string :as str]
            [commit-tree :as ct]))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn tree-entry-formatter
  "given a single entry from within a tree object, returns formatted string"
  [header-bytes address-bytes]
  (let [header-string-list (-> header-bytes fio/bytes->str (str/split #" "))
        mode (first header-string-list)
        name (second header-string-list)
        address (to-hex-string address-bytes)
        mode (if (= "40000" mode)
               (str "0" mode)
               mode)
        type (if (= mode "04000")
               "tree"
               "blob")]
    (str mode " " type " " address "\t" name "\n")))

(defn format-tree-output
  [content-bytes]
  (let [split-bytes (->> content-bytes
                         (partition-by (partial = 0x00))
                         (take-nth 2))
        len-1 (-> split-bytes count (- 1))]
    (loop [n 1
           header-bytes (first split-bytes)
           string ""]
      (if (> n len-1)
        string
        (let [all-info (->> (nth split-bytes n) (split-at 20))
              address-bytes (first all-info)
              next-header-bytes (second all-info)]
          (recur (inc n) next-header-bytes (str string (tree-entry-formatter header-bytes address-bytes))))))))

(defn cat-file
  "function for handling cat-file command"
  [args dir db]
  (let [switch (first args)
        address (second args)
        get-path #(str dir db "/objects/" (subs % 0 2) "/" (subs % 2))
        get-content-bytes #(->> %
                                 get-path
                                 fio/unzip
                                 (fio/split-at-byte (byte 0x00))
                                 second)]
    (cond
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not= switch "-p") (not= switch "-t")) (println "Error: the -p or -t switch is required")
      (nil? address) (println "Error: you must specify an address")
      (not (.exists (io/as-file (get-path address)))) (println "Error: that address doesn't exist")
      :else (case switch
              "-p" (if (= (ct/get-object-type address dir db) "tree")
                     (->> address
                          get-content-bytes
                          format-tree-output
                          print)
                     (->> address
                          get-content-bytes
                          (map char)
                          (apply str)
                          print))
              "-t" (-> address
                       (ct/get-object-type dir db)
                       println)))))
