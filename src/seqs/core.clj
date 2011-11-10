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

(h/defsnippet mk-table "seqs/html/table.html" [:table]
  [data-strs results]
  [:thead :th.crt-b] (h/clone-for [sn data-strs]
                                  (h/content sn))
  [:tbody :tr] (h/clone-for [fsym pred-syms]
                            [:th.crt-a] (h/content (str fsym))
                            [:td] (h/clone-for [a (get results (str fsym))]
                                               (h/content (str a)))))

(h/deftemplate mk-page "seqs/html/main.html"
  [table-sn]
  [:#cartesian] (h/content table-sn))

(defn -main
  [& args]
  (let [results (run-all pred-syms data-snips)
        table-sn (mk-table data-snips results)
        page (mk-page table-sn)]
    (println (apply str page))))
