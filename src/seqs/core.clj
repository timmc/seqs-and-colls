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
    [fn-syms data-strs results]

    [:thead :th.crt-b]
    (h/clone-for [sn data-strs]
                 [:code] (h/content sn))
    
    [:tbody :tr]
    (h/clone-for [fsym fn-syms]
                 [:th.crt-a :code] (h/content (name fsym))
                 [:td] (h/clone-for [a (get results (name fsym))]
                                    [:img] (xf-img a)))))

(h/deftemplate mk-page "seqs/html/main.html"
  [tbl-collseq tbl-colltypes]
  [:#tbl-collseq] (h/content tbl-collseq)
  [:#tbl-colltypes] (h/content tbl-colltypes))

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
              (table-for '[sequential? associative?]
                         [d-lazyseq d-list d-vec d-map d-set]))]
    (println (apply str page))))
