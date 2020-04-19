(ns endpoints
  (:require [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [rev-parse :as rp]
            [clojure.string :as str]
            [rev-list :as rl]
            [log :as lg]))

(def main-body {:status 200 :headers {"Content-Type" "text/html"}})

(def error-404 {:status 404})

(defn add-body [html]
  (assoc main-body :body html))

(defn branch-html
  [branch dir db]
  (let [path (str dir db "/refs/heads/" branch)
        to-html #(html5 [:head [:title branch]] [:body %])]
    (if (.exists (io/file path))
      (->> branch
           (rl/ref-to-chain dir db log/get-commit-chain-log)
           (map (fn [tuple] (lg/format-oneline-output (first tuple) (second tuple))))
           (map #(subs % 0 (- (count %) 1)))
           (map #(vec [:li [:a {:href (str "/branches/" (subs % 0 7))} (subs % 0 7)] (subs % 7)]))
           (reduce (fn [vec val] (conj vec val)) [:ul {:class "commit-list"}])
           to-html
           add-body
           )
      error-404)))

(defn head-html
  [dir db]
  (let [head-ref (-> (-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last))
        head-html [:div {:class "head-info"} "HEAD points to ref " [:a {:href (str "/branches/" head-ref)} head-ref]]
        branches (->> (str dir db "/refs/heads")
                      io/file
                      .listFiles
                      (sort-by #(.getName %))
                      (map #(.getName %)))
        add-branch (fn [vector val] (conj vector [:li [:a {:href (str "/branches/" val)} val]]))
        branch-html (reduce add-branch [:ul {:class "branch-list"}] branches)]
    (add-body (html5 [:head [:title "Branches"]] [:body head-html branch-html]))))

