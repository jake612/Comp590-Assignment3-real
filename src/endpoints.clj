(ns endpoints
  (:require [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [rev-parse :as rp]
            [clojure.string :as str]
            [rev-list :as rl]
            [log :as lg]
            [get_address :as ga]
            [commit-tree :as ct]
            [cat-file :as cf]))

(def main-body {:status 200 :headers {"Content-Type" "text/html"}})

(def error-404 {:status 404})

(defn redirect
  [obj-type address]
  {:status 302 :headers {"Location" (str "/" obj-type "/" address)}})

(defn format<>
  [line]
  (-> line (str/replace #"<" "&lt;") (str/replace #">" "&gt;")))

(defn add-body [html]
  (assoc main-body :body html))

(defn duplicate-html
  [target-info dir db]
  (let [target-type (map #(vec [% (ct/get-object-type % dir db)]) (second target-info))
        target-html (reduce (fn [html [addr type]] (conj html [:li [:a {:href (str "/" type "/" addr)} addr] (str " (" type ")")])) [:ul {:class "disambiguation-list"}] target-type)]
    {:status 300
     :headers {"Content-Type" "text/html"}
     :body   (html5 [:head [:title "Multiple Choices"]]
                    [:body
                     [:p "The given address prefix is ambiguous. Please disambiguate your intent by choosing from the following options."]
                     target-html])}))

(defn get-path [dir db address]
  (str dir db "/objects/" (subs address 0 2) "/" (subs address 2)))

(defn obj-middleware
  [type func address dir db]
  (let [target-addresses (ga/search-address address dir db)
        count-addresses (first target-addresses)
        full-address (first (second target-addresses))
        file-type (ct/get-object-type full-address dir db)]
    (cond
      (or (< (count address) 4) (= 0 count-addresses)) error-404
      (< 1 count-addresses) (duplicate-html target-addresses dir db)
      :else (if (not (= file-type type))
              (redirect file-type address)
              (add-body (func address full-address dir db))))))

(defn blob-body
  [address full-address dir db]
  (let [contents (->> full-address
                      (get-path dir db)
                      cf/get-content-bytes
                      (map char)
                      (apply str))]
    (html5 [:head [:title address]]
           [:body
            [:h1 (str "Blob " address)]
            [:pre contents]])))

(defn tree-body
  [address full-address dir db]
  (let [contents (->> full-address
                      (get-path dir db)
                      cf/get-content-bytes
                      cf/format-tree-output)
        lines (str/split contents #"\n")
        formatter (fn [[code type addr name]]
                    (vec [:li
                          [:tt
                           (str code " " type " ")
                           [:a {:href (str "/" type "/" addr)} addr]
                           (str " " name)]]))
        formatted-lines (->> lines
                             (map #(str/replace % #"\t" " "))
                             (map #(str/split % #" "))
                             (map formatter))]
    (html5 [:head [:title address]]
           [:body
            [:h1 (str "Tree " address)]
            [:ul {:class "tree-entries"}
             formatted-lines]])))

(defn commit-body
  [address full-address dir db]
  (let [contents (->> full-address
                      (get-path dir db)
                      cf/get-content-bytes
                      (map char)
                      (apply str))
        lines (str/split contents #"\n")
        tree-address (-> lines first (str/split #" ") second)
        parents (->> lines
                     (filter #(-> (str/split % #" ") first (= "parent")))
                     (map #(-> (str/split % #" ") second))
                     (map #(vec [:div {:class "parent"} [:a {:href (str "/commit/" %)} (str "parent " %)]])))
        author-pos (->> parents count (+ 1))
        message (->> author-pos
                     (+ 3)
                     (subvec lines)
                     (reduce (fn [conc line] (str conc line "\n")) ""))]
    (html5 [:head [:title address]]
           [:body
            [:h1 (str "Commit " address)]
            [:div {:class "tree"} [:a {:href (str "/tree/" tree-address)} (str "tree " tree-address)]]
            parents
            [:div {:class "author"} (->> author-pos (nth lines) format<>)]
            [:div {:class "committer"} (->> author-pos (+ 1) (nth lines) format<>)]
            [:pre {:class "message"} message]])))


(defn branch-html
  [branch dir db]
  (let [path (str dir db "/refs/heads/" branch)
        to-html #(html5 [:head [:title branch]] [:body %])]
    (if (.exists (io/file path))
      (->> branch
           (rl/ref-to-chain dir db log/get-commit-chain-log)
           (map (fn [tuple] (lg/format-oneline-output (first tuple) (second tuple))))
           (map #(subs % 0 (- (count %) 1)))
           (map #(vec [:li [:a {:href (str "/commit/" (subs % 0 7))} (subs % 0 7)] (subs % 7)]))
           (reduce (fn [vec val] (conj vec val)) [:ul {:class "commit-list"}])
           to-html
           add-body)
      error-404)))

(defn head-html
  [dir db]
  (let [head-ref (-> (-> (str dir db "/HEAD") rp/get-contents-no-nl (str/split #"/") last))
        head-html [:div {:class "head-info"} "HEAD points to ref " [:a {:href (str "/branch/" head-ref)} head-ref]]
        branches (->> (str dir db "/refs/heads")
                      io/file
                      .listFiles
                      (sort-by #(.getName %))
                      (map #(.getName %)))
        add-branch (fn [vector val] (conj vector [:li [:a {:href (str "/branch/" val)} val]]))
        branch-html (reduce add-branch [:ul {:class "branch-list"}] branches)]
    (add-body (html5 [:head [:title "Branches"]] [:body head-html branch-html]))))

