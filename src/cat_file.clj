(ns cat-file
  (:require [file-io :as fio]
            [clojure.string :as str]
            [commit-tree :as ct]
            [get_address :as ga]))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn tree-entry-formatter
  "given a single entry from within a tree object, returns formatted string"
  [header-bytes address-bytes]
  (let [header-string-list (-> header-bytes fio/bytes->str (str/split #" "))
        mode (first header-string-list)
        name (second header-string-list)
        address (to-hex-string address-bytes)
        mode (if (= "40000" mode)
               (str "0" mode)
               mode)
        type (if (= mode "040000")
               "tree"
               "blob")]
    (str mode " " type " " address "\t" name "\n")))

(defn format-tree-output
  [content-bytes]
  (let [split-bytes (->> content-bytes
                         (partition-by (partial = 0x00))
                         (take-nth 2))
        len-1 (-> split-bytes count (- 1))]
    (loop [n 1
           header-bytes (first split-bytes)
           string ""]
      (if (> n len-1)
        string
        (let [all-info (->> (nth split-bytes n) (split-at 20))
              address-bytes (first all-info)
              next-header-bytes (second all-info)]
          (recur (inc n) next-header-bytes (str string (tree-entry-formatter header-bytes address-bytes))))))))

(defn get-content-bytes
  [path]
  (->> path
       fio/unzip
       (fio/split-at-byte (byte 0x00))
       second))

(defn cat-file
  "function for handling cat-file command"
  [args dir db]
  (let [switch (first args)
        given-addr (second args)
        info (ga/search-address given-addr dir db)
        address (first (second info))
        matching-addresses (first info)
        get-path #(str dir db "/objects/" (subs % 0 2) "/" (subs % 2))]
    (cond
      (fio/check-db-missing dir db) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not= switch "-p") (not= switch "-t")) (println "Error: the -p or -t switch is required")
      (nil? given-addr) (println "Error: you must specify an address")
      (< (count given-addr) 4) (println (format "Error: too few characters specified for address '%s'" given-addr))
      (not (= 1 matching-addresses))(ga/addr-loc-error-handler given-addr matching-addresses "Error: that address doesn't exist")
      :else (println info))))

(comment (case switch
           "-p" (if (= (ct/get-object-type address dir db) "tree")
                  (->> address
                       get-path
                       get-content-bytes
                       format-tree-output
                       print)
                  (->> address
                       get-path
                       get-content-bytes
                       (map char)
                       (apply str)
                       print))
           "-t" (-> address
                    (ct/get-object-type dir db)
                    println)))