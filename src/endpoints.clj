(ns endpoints
  (:require [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [rev-parse :as rp]
            [clojure.string :as str]))

(def main-body {:status 200 :headers {"Content-Type" "text/html"}})

(defn add-body [html]
  (assoc main-body :body html))

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
        head-html [:div {:class "head-info"} "Head points to ref " [:a {:href (str "/branches/" head-ref)} head-ref]]
        branches (->> (str dir db "/refs/heads")
                      io/file
                      .listFiles
                      (sort-by #(.getName %))
                      (map #(.getName %)))
        add-branch (fn [vector val] (conj vector [:li [:a {:href (str "/branches/" val)} val]]))
        branch-html (reduce add-branch [:ul {:class "branch-list"}] branches)]
    (add-body (html5 [:head [:title "Branches"]] [:body head-html branch-html]))))

;:body (html5 [:head [:title "Branches"]] (get-branches-html dir db))