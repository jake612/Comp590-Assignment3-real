(ns endpoints
  (:require [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [rev-parse :as rp]
            [clojure.string :as str]))

(defn get-branches-html
  [dir db]
  (let [branches (->> (str dir db "/refs/heads")
                      io/file
                      .listFiles
                      (sort-by #(.getName %))
                      (map #(.getName %)))
        add-branch (fn [vector val] (conj vector [:p val]))]
    (reduce add-branch [:body] branches)))

(defn head-html
  [dir db]
  (let [head-ref (-> (-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last))
        head-html [:div {:class "head-info"} (str "Head points to " head-ref)]
        branches (->> (str dir db "/refs/heads")
                      io/file
                      .listFiles
                      (sort-by #(.getName %))
                      (map #(.getName %)))
        add-branch (fn [vector val] (conj vector [:li val]))
        branch-html (reduce add-branch [:ul] branches)]
    (html5 [:head [:title "Branches"]] [:body head-html branch-html])))

;:body (html5 [:head [:title "Branches"]] (get-branches-html dir db))