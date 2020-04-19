(ns endpoints
  (:require [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [rev-parse :as rp]
            [clojure.string :as str]
            [rev-list :as rl]
            [log :as lg]
            [get_address :as ga]
            [commit-tree :as ct]))

(def main-body {:status 200 :headers {"Content-Type" "text/html"}})

(def error-404 {:status 404})

(defn redirect
  [obj-type address]
  {:status 302 :headers {"Location" (str "/" obj-type "/" address)}})

(defn add-body [html]
  (assoc main-body :body html))

(defn duplicate-html
  [target-info dir db]
  (let [target-type (map #(vec [% (ct/get-object-type % dir db)]) (second target-info))
        target-html (reduce (fn [html [addr type]] (conj html [:li [:a {:href (str "/" type "/" addr)} addr] (str " (" type ")")])) [:ul {:class "disambiguation-list"}] target-type)]
    {:status 300
     :body   (html5 [:head [:title "Multiple Choices"]]
                    [:body
                     [:p "The given address prefix is ambiguous. Please disambiguate your intent by choosing from the following options."]
                     target-html])}))

(defn commit-html
  [address dir db]
  (let [target-addresses (ga/search-address address dir db)
        count-addresses (first target-addresses)
        full-address (first (second target-addresses))
        file-type (ct/get-object-type full-address dir db)]
    (cond
      (or (< (count address) 4) (= 0 count-addresses)) error-404
      (< 1 count-addresses) (duplicate-html target-addresses dir db)
      :else (if (not (= file-type "commit"))
              (redirect file-type full-address)
              (add-body (html5 [:body [:div full-address]]))))))

(defn branch-html
  [branch dir db]
  (let [path (str dir db "/refs/heads/" branch)
        to-html #(html5 [:head [:title branch]] [:body %])]
    (if (.exists (io/file path))
      (->> branch
           (rl/ref-to-chain dir db log/get-commit-chain-log)
           (map (fn [tuple] (lg/format-oneline-output (first tuple) (second tuple))))
           (map #(subs % 0 (- (count %) 1)))
           (map #(vec [:li [:a {:href (str "/commits/" (subs % 0 7))} (subs % 0 7)] (subs % 7)]))
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

