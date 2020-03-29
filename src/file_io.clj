(ns file-io
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.zip InflaterInputStream)
           (java.io ByteArrayOutputStream)))

(defn unzip
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

;; Note that if given binary data this will fail with an error message
;; like:
;; Execution error (IllegalArgumentException) at ,,,.
;; Value out of range for char: -48
(defn bytes->str [bytes]
  (->> bytes (map char) (apply str)))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))

(defn check-db-missing
  "checks if the given database doesn't exist"
  [dir db]
  (-> (io/file dir db)
      .isDirectory
      not))



