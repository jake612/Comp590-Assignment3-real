(ns idiot
  (:require [clojure.java.io :as io]
            [hash-object]
            [cat-file]
            [init]
            [help]
            [write-wtree]
            [commit-tree]
            [rev-parse]
            [switch]
            [branch]
            [commit]
            [rev-list]
            [log]
            [explore]))

(defn handle-main-switches
  [args dir db]
  (let [num-args (count args)
        command (first args)
        check-first (fn [func] (if (or (= "-h" (second args)) (= "--help" (second args)))
                                 (help/help command)
                                 (func (rest args) dir db)))
        handle-r-switch (fn [[targetdir & rargs]] (cond
                                                    (nil? targetdir) (println "Error: the -r switch needs an argument")
                                                    (not (.exists (io/as-file targetdir))) (println "Error: the directory specified by -r does not exist")
                                                    :else (handle-main-switches rargs (str targetdir "/") db)))
        handle-d-switch (fn [[targetdb & rargs]] (cond
                                                   (nil? targetdb) (println "Error: the -d switch needs an argument")
                                                   :else (handle-main-switches rargs dir targetdb)))]
    (cond
      (= command "-r") (handle-r-switch (rest args))
      (= command "-d") (handle-d-switch (rest args))
      (or (= num-args 0) (= command "-h") (= command "--help")) (help/help "idiot")
      (= command "help") (help/help (second args))
      (= command "init") (check-first init/init)
      (= command "hash-object") (check-first hash-object/hash-object)
      (= command "cat-file") (check-first cat-file/cat-file)
      (= command "write-wtree") (check-first write-wtree/write-wtree)
      (= command "commit-tree") (check-first commit-tree/commit-tree)
      (= command "rev-parse") (check-first rev-parse/rev-parse)
      (= command "switch") (check-first switch/switch)
      (= command "branch") (check-first branch/branch)
      (= command "commit") (check-first commit/commit)
      (= command "rev-list") (check-first rev-list/rev-list)
      (= command "log") (check-first log/log)
      (= command "explore") (check-first explore/explore)
      :else (println "Error: invalid command"))))

(defn -main
  "Main method for handling CLI"
  [& args]
  (handle-main-switches args "./" ".idiot"))

