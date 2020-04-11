(ns explore
  (:require [file-io :as fio]
            [ring.adapter.jetty :refer [run-jetty]]
            [hiccup.page :refer [html5]]
            [clojure.java.io :as io]))

(def default-port 3000)

(defn print-server-start
  [port]
  (println (format "Starting server on port %s." port)))

(defn get-branches-html
  [dir db]
  (let [branches (->> (str dir db "/refs/heads")
                      io/file
                      .listFiles
                      (sort-by #(.getName %))
                      (map #(.getName %)))
        add-branch (fn [vector val] (conj vector [:p val]))]
    (reduce add-branch [:body] branches)))

(defn request-handler
  [request dir db]
  request
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html5 [:head [:title "Branches"]] (get-branches-html dir db))})

(defn start-server
  [port dir db]
  (print-server-start port)
  (run-jetty #(request-handler % dir db) {:port port}))

(defn handle-switch
  [port dir db]
  (cond
    (nil? port) (println "Error: you must specify a numeric port with '-p'.")
    (nil? (re-matches #"[0-9]+" port)) (println "Error: the argument for '-p' must be a non-negative integer.")
    :else (start-server (Integer/parseInt port) dir db)))

(defn explore
  [args dir db]
  (let [[switch port] args]
    (cond
      (fio/check-db-missing dir db) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= switch "-p") (handle-switch port dir db)
      :else (start-server default-port dir db))))