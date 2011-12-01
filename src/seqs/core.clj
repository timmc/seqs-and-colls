(ns seqs.core
  "Create a seqs vs. colls comparison page with generated data tables."
  (:require [net.cgrand.enlive-html :as h]))

(def d-lazyseq "(range)")
(def d-list "'(1 2 3)")
(def d-list-empty "()")
(def d-vec "[4 5 6]")
(def d-vec-empty "[]")
(def d-map "{:a 1, :b 2}")
(def d-set "#{7 8 9}")
(def d-string "\"hello\"")
(def d-string-empty "\"\"")
(def d-nil "nil")
(def d-other "17")

;;;; Computing results

(defn make-result
  "Wraps the function to produce true, false, or :e."
  [f]
  #(try
     (if (apply f %&) true false)
     (catch Exception e :e)))

(defn run-all
  "Eval data strings and run them through the functions, producing a map of
   function name strings to seqs of return values. Elements of fsos may be
   symbols that resolve to functions or strings that eval to functions."
  [fsos data-strs]
  (let [data (map (comp eval read-string) data-strs)]
    (into {}
          (for [s fsos]
            (let [f (make-result (cond
                                  (symbol? s)
                                  (deref (resolve s))
                                  
                                  (instance? String s)
                                  (eval (read-string s))))]
              [(name s)
               (map f data)])))))


;;;; HTML generation

(let [;; These are maps from result values to image attributes
      im-alt {true "true", false "false", :e "exception"}
      im-title {true "Logical true",
                false "Logical false",
                :e "Throws exception"}
      im-src {true "yes.png", false "no.png", :e "warning.png"}
      ;; Transforms an img node using a result value
      xf-img #(h/set-attr :alt (im-alt %) :title (im-title %) :src (im-src %))]
  (h/defsnippet mk-table "seqs/html/table.html" [:table]
    [fsos data-strs results]
    
    [:thead :th.crt-b]
    (h/clone-for [sn data-strs]
                 [:code] (h/content sn))
    
    [:tbody :tr]
    (h/clone-for [sos fsos]
                 [:th.crt-a :code] (h/content (name sos))
                 [:td] (h/clone-for [a (get results (name sos))]
                                    [:img] (xf-img a)))))

(h/deftemplate mk-page "seqs/html/main.html"
  [tbl-collseq tbl-seq tbl-seq-remain tbl-colltypes tbl-eqpart]
  [:#tbl-collseq] (h/content tbl-collseq)
  [:#tbl-seq] (h/content tbl-seq)
  [:#tbl-seq-remain] (h/content tbl-seq-remain)
  [:#tbl-colltypes] (h/content tbl-colltypes)
  [:#tbl-eqpart] (h/content tbl-eqpart))

(defn table-for
  "Produce a node tree from a collection of function symbol-or-strings and a
collection of values."
  [fns data]
  (let [results (run-all fns data)]
    (mk-table fns data results)))

(defn -main
  "Print HTML to stdout."
  [& args]
  (let [page (mk-page
              (table-for '[coll? seq?]
                         [d-lazyseq d-list d-vec d-map d-set
                          d-string d-nil d-other])
              (table-for '[seq empty?]
                         [d-lazyseq d-list d-vec d-map d-set d-string
                          d-list-empty d-vec-empty d-string-empty d-nil
                          d-other])
              (table-for '[first next rest]
                         ["[1 2]" "[1]" "[]" "nil" "17"])
              (table-for '[coll? counted? sequential? associative?]
                         [d-lazyseq d-list d-vec d-map d-set d-string d-nil])
              (table-for ["#(= () %)" "#(= [] %)" "#(= {} %)" "#(= #{} %)"]
                         ["()" "[]" "{}" "#{}"]))]
    (println (apply str page))))
