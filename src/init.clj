(ns init
  (:require [clojure.java.io :as io]))

(defn init
  "Function to initialize a new database"
  [args dir db]
  (cond
    (> (count args) 0) (println "Error: init accepts no arguments")
    :else (if (.isDirectory (io/file dir db))
            (println (format "Error: %s directory already exists" db))
            (do (io/make-parents (str dir db "/objects/sample.txt"))
                (println (format "Initialized empty Idiot repository in %s directory" db))))))