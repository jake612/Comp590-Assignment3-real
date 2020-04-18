(ns explore
  (:require [file-io :as fio]
            [ring.adapter.jetty :refer [run-jetty]]
            [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [endpoints :as ep]))

(def default-port 3000)

(defn print-server-start
  [port]
  (println (format "Starting server on port %s." port)))

(defn request-handler
  [request dir db]
  (let [main-body {:status 200 :headers {"Content-Type" "text/html"}}
        endpoint (:uri request)
        request-method (:request-method request)
        add-body #(assoc main-body :body %)]
    (cond
      (and (= endpoint "/") (= request-method :get)) (add-body (ep/head-html dir db))
      :else (add-body (html5 [:head [:title "Error"]] [:p "endpoint not found"])))))

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

;:body (html5 [:head [:title "Branches"]] (get-branches-html dir db))