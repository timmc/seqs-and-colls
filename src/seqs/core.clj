(ns seqs.core
  (:require [net.cgrand.enlive-html :as h]))

(def data-snips
  ["(range) ; lazy seq"
   "'(4 5 6) ; list"
   "() ; empty list"
   "[1 2 3] ; vector"
   "[] ; empty vec"
   "{:a 1 :b 2} ; map"
   "#{7 8 9} ; set"
   "\"hello\" ; string"
   "nil ; nil/null"
   "17 ; other"])

(def pred-syms
  '[coll?
    sequential?
    associative?
    seq?
    seq
    empty?])

(defn make-result
  "Wraps the function to produce true, false, or :e."
  [f]
  #(try
     (if (apply f %&) true false)
     (catch Exception e :e)))

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

(let [im-alt {true "true", false "false", :e "exception"}
      im-title {true "Logical true",
                false "Logical false",
                :e "Throws exception"}
      im-src {true "yes.png", false "no.png", :e "warning.png"}
      xf-img #(h/set-attr :alt (im-alt %) :title (im-title %) :src (im-src %))]
  (h/defsnippet mk-table "seqs/html/table.html" [:table]
    [data-strs results]

    [:thead :th.crt-b]
    (h/clone-for [sn data-strs]
                 (h/content sn))
    
    [:tbody :tr]
    (h/clone-for [fsym pred-syms]
                 [:th.crt-a] (h/content (name fsym))
                 [:td] (h/clone-for [a (get results (name fsym))]
                                    [:img] (xf-img a)))))

(h/deftemplate mk-page "seqs/html/main.html"
  [table-sn]
  [:#cartesian] (h/content table-sn))

(defn -main
  [& args]
  (let [results (run-all pred-syms data-snips)
        table-sn (mk-table data-snips results)
        page (mk-page table-sn)]
    (println (apply str page))))
