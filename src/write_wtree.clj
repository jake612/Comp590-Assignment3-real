(ns write-wtree
  (:require [clojure.java.io :as io]
            [hash-handler :as hh]
            [hash-object :as ho]
            [clojure.string :as str]
            [git]
            [byte-array :as ba]))

(defn write-object
  [object-bytes db]
  (let [address (sha/string object-bytes)
        path-of-destination-file (str db "/objects/" (subs address 0 2) "/" (subs address 2))]
    (when (not (.exists (io/as-file path-of-destination-file)))
      (do (io/make-parents path-of-destination-file)
          (io/copy (ho/zip-str object-bytes) (io/file path-of-destination-file))))
    address))

(defn hex-digits->byte
  [[dig1 dig2]]
  ;; This is tricky because something like "ab" is "out of range" for a
  ;; Byte, because Bytes are signed and can only be between -128 and 127
  ;; (inclusive). So we have to temporarily use an int to give us the room
  ;; we need, then adjust the value if needed to get it in the range for a
  ;; byte, and finally cast to a byte.
  (let [i (Integer/parseInt (str dig1 dig2) 16)
        byte-ready-int (if (< Byte/MAX_VALUE i)
                         (byte (- i 256))
                         i)]
    (byte byte-ready-int)))

(defn from-hex-string
  [hex-str]
  (byte-array (map hex-digits->byte (partition 2 hex-str))))

(defn blob-entry-formatter
  "generates a blob entry for use in a tree"
  [file db]
  (ho/write-blob (.getAbsolutePath file) "" db)
  (ba/concat (.getBytes (str "100644 " (.getName file) "\000")) (from-hex-string (hh/sha1-sum (hh/blob-data file)))))

(defn tree-entry-formatter
  [name address]
  (ba/concat (.getBytes (str "40000 " name "\000")) (from-hex-string address)))

(defn generate-tree-entry
  "generate tree entry"
  [entries db]
  (let [length (reduce + 0 (map count entries))
        cat-entries (apply concat entries)
        object-bytes (-> (str "tree " length "\000")
                         .getBytes
                         (concat cat-entries)
                         byte-array)]
    (write-object object-bytes db)))


(defn gen-tree
  "Function for recursively generating a tree given a directory"
  [level db current-dir]
  (let [files (file-seq current-dir)
        sort-files (sort-by #(.getName %) (rest files))
        filter-files (filter #(= level (count (re-seq #"\\" (.getPath %)))) sort-files)
        entries (for [file filter-files] (if (.isDirectory file)
                                           (tree-entry-formatter (.getName file) (gen-tree (inc level) db file))
                                           (blob-entry-formatter file db)))]
    (generate-tree-entry (vec entries) db)))

(defn write-wtree
  "Function handles the write-wtree command"
  [args dir db]
  (cond
    (> (count args) 0) (println "Error: write-wtree accepts no arguments")
    (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
    :else (->> (io/file dir) (gen-tree (count (re-find (re-pattern "\\\\") dir))  db) println)))



