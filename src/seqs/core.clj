(ns seqs.core
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

(defn make-result
  "Wraps the function to produce true, false, or :e."
  [f]
  #(try
     (if (apply f %&) true false)
     (catch Exception e :e)))

(defn run-all
  "Eval data strings and run them through the functions, producing a map of
   function name strings to seqs of return values. Elements of foss may be symbols
   that resolve to functions or strings that eval to functions."
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

(let [im-alt {true "true", false "false", :e "exception"}
      im-title {true "Logical true",
                false "Logical false",
                :e "Throws exception"}
      im-src {true "yes.png", false "no.png", :e "warning.png"}
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
  [tbl-collseq tbl-colltypes tbl-eqpart]
  [:#tbl-collseq] (h/content tbl-collseq)
  [:#tbl-colltypes] (h/content tbl-colltypes)
  [:#tbl-eqpart] (h/content tbl-eqpart))

(defn table-for
  [fns data]
  (let [results (run-all fns data)]
    (mk-table fns data results)))

(defn -main
  [& args]
  (let [page (mk-page
              (table-for '[coll? seq?]
                         [d-lazyseq d-list d-vec d-map d-set
                          d-string d-nil d-other])
              (table-for '[coll? sequential? associative?]
                         [d-lazyseq d-list d-vec d-map d-set])
              (table-for ["#(= () %)" "#(= [] %)" "#(= {} %)" "#(= #{} %)"]
                         ["()" "[]" "{}" "#{}"]))]
    (println (apply str page))))
