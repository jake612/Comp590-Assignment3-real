(ns hash-handler
  (:import (java.security MessageDigest)))

(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))

(defn blob-data
  "function to compute the content address for a given file"
  [file]
  (let [contents (slurp file)]
    (str "blob " (count contents) "\000" contents)))