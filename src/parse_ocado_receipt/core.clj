(ns parse-ocado-receipt.core
  (:gen-class)
  (:require [clj-time.core :as t]
            [clj-time.format :as tf]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [instaparse.core :as insta])
  (:import [java.time DayOfWeek]))

(def delivery-date-formatter (tf/formatter "dd/MM/yyyy"))

(insta/defparser receipt-parser (slurp (io/resource "ocado-parser.bnf")))

(defn handle-storage [storage-details]
  (println storage-details)
  (assoc
   (when (= :expiring (first storage-details))
     {:expiring (last (second storage-details))}
     ))
  {})

(defn- handle-details [details]
  (cond
    (and (coll? details) (string? (first details)))
    (str/join " "details)

    (and (vector? (first details)) (= 2 (count (first details))))
    (into {} details)

    :default
    details))

(defn product-item [item-details expiring]
  ;(println "details: " item-details)
  (->> (map (fn [[detail-key & details :as detail]]
              (if-not (= 2 (count detail))
                [detail-key (handle-details details)]
                detail)) item-details)
       (cons [:expiring expiring])
       (into {})))

(defn- exp->int [delivery-day-of-week exp]
  (or (and (= "tomorrow" exp) 1)
      (let [exp-day-of-week (.getValue (DayOfWeek/valueOf (str/upper-case exp)))]
        (- (if (> exp-day-of-week delivery-day-of-week)
             exp-day-of-week
             (+ 7 exp-day-of-week)) delivery-day-of-week))))

(defn postprocess-receipt [parsed-receipt]
  (println "post processing" (first (drop 2 parsed-receipt)))
  (let [order (atom {})
        delivery-date (atom nil)
        curr-storage-type (atom nil)
        curr-expiring     (atom nil)]
    (walk/prewalk
     (fn [node]
       (when (vector? node)
         (cond

           (= :customer (first node))
           (swap! order assoc :customer (->> (rest node)
                                              (map second)
                                              (str/join " ")))

           (= :order-number (first node))
           (swap! order assoc :order-number (last node))

           (= :delivery-date (first node))
           (let [delivery-d (->> (rest node)
                                 second
                                 last
                                 (tf/parse delivery-date-formatter))]
             (reset! delivery-date delivery-d)
             (swap! order assoc :delivery-date delivery-d))

           (= :order-summary (first node))
           (swap! order assoc :order-summary (->> (rest node)
                                                  (into {})))

           (= :storage-type (first node))
           (let [storage-type (->> (last node) first)]
             (reset! curr-storage-type storage-type )
             (reset! curr-expiring nil)
             (swap! order assoc storage-type []))

           (= :expiring (first node))
           (let [exp (->> (second node)
                          last)]
             (reset!
              curr-expiring
              (if (string? exp)
                (t/plus @delivery-date (t/days (exp->int (t/day-of-week @delivery-date) exp)))
                exp)))

           (= :product-item (first node))
           (swap! order update @curr-storage-type conj (product-item (rest node) @curr-expiring))))
       node)
     parsed-receipt)
    (deref order)))

(defn parse-receipt [parser receipt]
  (insta/parse parser receipt))

;; need to find a way to do this, perhaps separate google apps script?!
;; (defn pdf->txt [pdf-file-name]
;;   @(http/put "http://givemetext.okfnlabs.org/tika/tika" {:body (slurp (io/resource pdf-file-name))
;;                                                          :headers {"Content-Type" "application/pdf"}}))

(defn preprocess-pdf-content [[file-name pdf-content]]
  [file-name
   (-> (str/replace pdf-content #"(?s)Everything as it should be\?.*Cost of goods" "Everything as it should be?\nCost of goods")
       (str/replace #"(?s)Offers savings\s+You've saved.*" "Offers savings\nYou've saved")
       (str/replace #"(?s)Price per item shown.*?IL[\d-]+" "")
       (str/replace #"(?s)Delivered.*?\(£\)" "")
       ;(str/replace #"\*" "")
       ;(str/replace "£" "")
       )])

(defn parse-receipts [parser text-file-names]
  (->> (map (juxt identity
                  (fn [text-file-name]
                    (slurp (io/resource text-file-name)))) text-file-names)
       (map preprocess-pdf-content)
       (reduce
        (fn [acc [file-name pdf-content]]
          (println "parsing file" file-name " order-num:" (re-find #"Order number: \d+" pdf-content))
          (conj acc (let [parsed-receipt (parse-receipt parser pdf-content)]
                      (println "parsing done" (first (drop 2 parsed-receipt)))
                      parsed-receipt)))
        [])))

(defn list-txt-files []
  (filter #(str/ends-with? % ".txt") (str/split (:out (sh/sh "ls" "resources/")) #"\n"))
  )

(defn -main [& args]
  (println "parsing starts")
  (parse-receipts receipt-parser (list-txt-files))
  (println "bye")
  (System/exit 0))
