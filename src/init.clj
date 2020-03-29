(ns init
  (:require [clojure.java.io :as io]))

(defn init
  "Function to initialize a new database"
  [args dir db]
  (cond
    (> (count args) 0) (println "Error: init accepts no arguments")
    (.isDirectory (io/file dir db)) (println (format "Error: %s directory already exists" db))
    :else (do (io/make-parents (str dir db "/objects/sample.txt"))
              (io/make-parents (str dir db "/refs/heads/sample.txt"))
              (spit (str dir db "/HEAD") "ref: refs/heads/master\n")
              (println (format "Initialized empty Idiot repository in %s directory" db)))))