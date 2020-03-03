(ns git
  (:require [byte-array :as ba]
            sha))

(defn with-header
  "Return the given data with a header prepended, as a byte array."
  [type data]
  (ba/concat
    (format "%s %d\000" type (count data))
    data))

(defn address
  "Return the address of the given object of the given type. The object is
  assumed not to have a header already."
  [type data-without-header]
  (sha/string (with-header type data-without-header)))

;;(println (git/address "tree" (str "100644 file\000" (new String (byte-array [0xd0 0x3e 0x24 0x25 0xcf 0x1c 0x82 0x61 0x6e 0x12 0xcb 0x43 0x0c 0x69 0xaa 0xa6 0xcc 0x08 0xff 0x84])))))