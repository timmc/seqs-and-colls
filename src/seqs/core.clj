(ns seqs.core
  "Create a seqs vs. colls comparison page with generated data tables.

Most of the page is static text, but there are some tables that show the
results of calling various unary functions with various values.

Functions are kept internally as symbols or strings so as to preserve
printability, and values are kept as strings for the same reason.

A result is a map of :t in #{true, false, :e} and :v which is a value, possibly
an exception."
  (:require [net.cgrand.enlive-html :as h]))

;; Some oft-used args
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

(defn resultify
  "Wrap f to produce a Result from calls to f."
  [f]
  #(try
     (let [v (apply f %&)]
       {:t (boolean v), :v v})
     (catch Exception e {:t :e, :v e})))

(defn cross-eval
  "Eval data strings and run them through the functions, producing a map of
   function name strings to seqs of return values. Elements of fns may be
   symbols that resolve to functions or strings that eval to functions."
  [fns data-strs]
  (let [data (map (comp eval read-string) data-strs)]
    (into {}
          (for [s fns]
            (let [f (resultify (cond
                                (symbol? s) (deref (resolve s))
                                (string? s) (eval (read-string s))))]
              [(name s)
               (map f data)])))))

(defn pr-short-str
  "Return a truncated pr-str of the val. Safe for large data structures."
  [x]
  (binding [*print-level* 3
            *print-length* 20]
    (let [s (try (pr-str x)
                 (catch Exception e (str "!!! " (.getMessage e))))]
      (if (< 60 (count s))
        (str (.substring ^String s 0 60) "...")
        s))))

(defmulti result-desc
  "Produce a result's description."
  :t)

(defmethod result-desc true
  [{v :v}]
  (str "Logical true: " (pr-short-str v)))

(defmethod result-desc false
  [{v :v}]
  (str "Logical false: " (pr-short-str v)))

(defmethod result-desc :e
  [{v :v}]
  (str "Exception: " (.getMessage ^Exception v)))

;;;; HTML generation

(let [xf-img ;; transforms an img node using a result value
      #(apply
        h/set-attr
        (condp = (:t %)
            true [:alt "true" :title (result-desc %) :src "yes.png"]
            false [:alt "false" :title (result-desc %) :src "no.png"]
            :e [:alt "exception" :title (result-desc %) :src "warning.png"]))]
  (h/defsnippet mk-table "seqs/html/table.html" [:table]
    [fns data-strs results]
    
    [:thead :th.crt-b]
    (h/clone-for [ds data-strs]
                 [:code] (h/content ds))
    
    [:tbody :tr]
    (h/clone-for [f fns]
                 [:th.crt-a :code] (h/content (name f))
                 [:td] (h/clone-for [a (get results (name f))]
                                    [:img] (xf-img a)))))

(defn table-for
  "Produce a node tree from a collection of function symbol-or-strings and a
   collection of values."
  [fns args]
  (let [results (cross-eval fns args)]
    (mk-table fns args results)))

(defn inject-table
  "Given a node tree and an evaluation set, inject the eval results into a table
in the layout."
  [nt eset]
  (let [tbl (table-for (:fns eset) (:args eset))
        sel [(keyword (str \# (:id eset)))]]
    (h/at nt sel (h/content tbl))))

(defn make-page
  "Generate a page (seq of strings) given a set of evaluations to perform."
  [eval-sets]
  (let [pg (h/html-resource "seqs/html/main.html")
        pg (reduce inject-table pg eval-sets)]
    (h/emit* pg)))

(def cartesians
  "A set of maps, each with an :id, a collection :fns, and a collection :args.
These are suitable for passing to table-for."
  #{{:id "tbl-collseq"
     :fns '[coll? seq?]
     :args [d-lazyseq d-list d-vec d-map d-set
            d-string d-nil d-other]}
    {:id "tbl-seq"
     :fns '[seq empty?]
     :args [d-lazyseq d-list d-vec d-map d-set d-string
            d-list-empty d-vec-empty d-string-empty d-nil
            d-other]}
    {:id "tbl-seq-remain"
     :fns '[first next rest]
     :args ["[1 2]" "[1]" "[]" "nil" "17"]}
    {:id "tbl-colltypes"
     :fns '[coll? counted? sequential? associative?]
     :args [d-list d-vec d-map d-set d-string
            d-nil d-lazyseq "(seq \"hello\")"]}
    {:id "tbl-eqpart"
     :fns ["#(= () %)" "#(= [] %)" "#(= {} %)" "#(= #{} %)"]
     :args ["()" "[]" "{}" "#{}"]}})

(defn -main
  "Print HTML to stdout."
  [& args]
  (let [page (make-page cartesians)]
    (println (apply str page))))
