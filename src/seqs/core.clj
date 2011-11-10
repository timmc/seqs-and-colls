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

(def pred-syms
  '[coll?
    seq?
    sequential?
    associative?
    seq
    empty?])

(defn make-result
  "Wraps the function to produce true, false, or :exception."
  [f]
  #(try
     (if (apply f %&) true false)
     (catch Exception e :exception)))

;; Take map of name-str to result-seq
(h/deftemplate table "seqs/table.html"
  [results]
  [:thead :th.val] (h/clone-for [v data]
                                (h/content (pr-str v)))
  [:tbody :tr] (h/clone-for [fsym pred-syms]
                            [:th.fn] (h/content (str fsym))
                            [:td] (h/clone-for [a (get results (str fsym))]
                                               (h/content (str a)))))

(defn -main
  [& args]
  (let [results (into {}
                      (for [s pred-syms]
                        (let [f (make-result (deref (resolve s)))]
                          [(name s)
                           (map f data)])))]
    (println (apply str (table results)))))
