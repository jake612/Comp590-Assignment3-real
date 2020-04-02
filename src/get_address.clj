(ns get_address
  (:require [clojure.java.io :as io]))

(defn search-address
  [addr dir db]
  (let [files (->> (subs addr 0 2)
                   (str dir db "/objects/")
                   io/file
                   .listFiles
                   (map #(.getName %)))
        initial-chars (subs addr 2)
        chars-len (count initial-chars)
        matching-files (filter #(= initial-chars (subs % 0 chars-len)) files)]
    [(count matching-files) (str (subs addr 0 2) (first matching-files))]))

(defn addr-loc-error-handler
  [addr len]
  (if (> len 1)
    (->> addr (format "Error: ambiguous match for address '%s'") println)
    (println "Error: that address doesn't exist")))


