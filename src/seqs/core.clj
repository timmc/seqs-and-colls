(ns seqs.core
  (:require [net.cgrand.enlive-html :as h]))

(def data-snips
  ["(seq [10 11 12])"
   "[1 2 3]"
   "'(4 5 6)"
   "{:a 1 :b 2}"
   "#{7 8 9}"
   "\"hello\""
   "[]"
   "()"
   "nil"
   "5"])

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

(defn run-all
  "Eval data strings and run them through the functions, producing a map of
   function name strings to seqs of return values."
  [fn-syms data-strs]
  (let [data (map (comp eval read-string) data-strs)]
    (into {}
          (for [s fn-syms]
            (let [f (make-result (deref (resolve s)))]
              [(name s)
               (map f data)])))))

;; Take map of name-str to result-seq
(h/deftemplate table "seqs/table.html"
  [data-strs results]
  [:thead :th.val] (h/clone-for [sn data-strs]
                                (h/content sn))
  [:tbody :tr] (h/clone-for [fsym pred-syms]
                            [:th.fn] (h/content (str fsym))
                            [:td] (h/clone-for [a (get results (str fsym))]
                                               (h/content (str a)))))

(defn -main
  [& args]
  (let [results (run-all pred-syms data-snips)]
    (println (apply str (table data-snips results)))))
