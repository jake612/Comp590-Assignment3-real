(ns get_address
  (:require [clojure.java.io :as io]))

(defn search-address
  [addr dir db]
  (try (let [files (->> (subs addr 0 2)
                        (str dir db "/objects/")
                        io/file
                        .listFiles
                        (map #(.getName %)))
             initial-chars (subs addr 2)
             chars-len (count initial-chars)
             matching-files (filter #(= initial-chars (subs % 0 chars-len)) files)
             mapped-files (map #(str (subs addr 0 2) %) matching-files)]
         (if (= 0 (count matching-files))
           [0 addr]
           [(count matching-files) mapped-files]))
       (catch Exception e e [0, nil])))

(defn addr-loc-error-handler
  [addr len error-msg]
  (if (> len 1)
    (->> addr (format "Error: ambiguous match for address '%s'") println)
    (println error-msg)))


