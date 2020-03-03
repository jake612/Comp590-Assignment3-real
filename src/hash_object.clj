(ns hash-object
  (:require [clojure.java.io :as io])
  (:require [hash-handler :as hh])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream)))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn print-address
  "prints hash address for given file"
  [file dir]
  (println (hh/sha1-sum (hh/blob-data (str dir file)))))

(defn write-blob
  "function takes an address and writes it to the .git database"
  [file dir db]
  (let [header+blob (hh/blob-data (str dir file))
        address (hh/sha1-sum header+blob)
        path-of-destination-file (str dir db "/objects/" (subs address 0 2) "/" (subs address 2))]
    (when (not (.exists (io/as-file path-of-destination-file)))
      (do (io/make-parents path-of-destination-file)
          (io/copy (zip-str header+blob) (io/file path-of-destination-file))))))

(defn hash-object
  "main function for handling the hash-object command"
  [args dir db]
  (let [check-exists #(.exists (io/as-file (str dir %)))]
    (cond
      (or (nil? (first args)) (and (= (first args) "-w") (nil? (second args)))) (println "Error: you must specify a file.")
      (not (.isDirectory (io/file dir db))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= (first args) "-w") (if (check-exists (second args))
                              (do (print-address (second args) dir) (write-blob (second args) dir db))
                              (println "Error: that file isn't readable"))
      :else (if (check-exists (first args))
              (print-address (first args) dir)
              (println "Error: that file isn't readable")))))