(ns seqs.core
  (:require [net.cgrand.enlive-html :as h]))

(def data
  [(seq [10 11 12])
   [1 2 3]
   '(4 5 6)
   {:a 1 :b 2}
   #{7 8 9}
   "hello"
   []
   ()
   nil
   5])

(def preds
  [coll?
   seq?
   seq
   empty?
   sequential?
   associative?])

(defn resultifier
  "Wraps the function to produce "
  [f]
  #(try
     (if (apply f %&) true false)
     (catch Exception e :exception)))

(h/deftemplate table "seqs/table.html"
  [results]
  [:thead :th.val] (h/clone-for [v data] (h/content (pr-str v))))

(defn -main
  [& args]
  (let [results (map #(map (resultifier %) data) preds)]
    (println (apply str (table results)))))