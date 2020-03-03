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

(defn open-file
  "function which gets the file contents from a hashed object"
  [path]
  (let [open #(with-open [input (-> % io/file io/input-stream)]
                (unzip input))]
    (open path)))

(defn open-file
  "function which gets the file contents from a hashed object"
  [path]
  (-> path
      unzip
      bytes->str))

(defn get-address
  "when given a hash and directory, returns the correct address"
  [directory hash]
  (str directory "/objects/" (subs hash 0 2) "/" (subs hash 2)))

(defn check-type
  "checks type of object"
  [file-path]
  (first (str/split (open-file file-path) #" ")))

