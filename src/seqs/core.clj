(ns seqs.core
  "Create a seqs vs. colls comparison page with generated data tables.

Most of the page is static text, but there are some tables that show the
results of calling various unary functions with various values.

Functions are kept internally as symbols or strings so as to preserve
printability, and values are kept as strings for the same reason.

A result is a map of :t in #{true, false, :e} and :v which is a value, possibly
an exception."
  (:require [net.cgrand.enlive-html :as h]))

;;;; Utilities

(defn mapply
  "A version of apply that takes a map as the last argument, using the keys and
values in interleaved order, (mapply + 1 2 3 {4 5}) -> (+ 1 2 3 4 5).
Behavior when given a vector is undefined."
  [f & args]
  (when (empty? args)
    (throw (IllegalArgumentException.
            "Must pass at least one argument map to mapply.")))
  (let [intervening (butlast args)
        tail (for [me (last args), korv me] korv)]
    (apply f (concat intervening tail))))

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

(defmulti result-attrs
  "Produce a result image's attributes."
  :t)

(defmethod result-attrs true
  [{v :v}]
  {:alt "true" :title (str "Logical true: " (pr-short-str v)) :src "yes.png"})

(defmethod result-attrs false
  [{v :v}]
  {:alt "false" :title (str "Logical false: " (pr-short-str v)) :src "no.png"})

(defmethod result-attrs :e
  [{v :v}]
  {:alt "exception"
   :title (str "Exception: " (.getMessage ^Exception v))
   :src "warning.png"})

;;;; HTML generation

(defn cd-munge
  "Munge a symbol for ClojureDocs URLs. 'coll? becomes \"coll_q\", etc."
  [s]
  (-> s str
      (.replaceAll "\\." "_dot")
      (.replaceAll "\\?" "_q")
      (.replaceAll "/" "_")))

(defn doc-url
  "Given a symbol, produce the 1.3.0 documentation URL."
  [s]
  (let [ns (or (.getNamespace s) "clojure.core")
        n (name s)]
    (str "http://clojuredocs.org/clojure_core/1.3.0/" ns "/" (cd-munge n))))

(defn fn-label
  "Given a fn as symbol or string, produce node tree that might be a link."
  [f]
  (if (symbol? f)
    {:tag :a
     :attrs {:href (doc-url f), :title "View on ClojureDocs"}
     :content (name f)}
    (name f)))

(h/defsnippet mk-table "seqs/html/table.html" [:table]
  [fns data-strs results]
  
  [:thead :th.crt-b]
  (h/clone-for [ds data-strs]
               [:code] (h/content ds))
  
  [:tbody :tr]
  (h/clone-for [f fns]
               [:th.crt-a :code] (h/content (fn-label f))
               [:td] (h/clone-for [a (get results (name f))]
                                  [:img] (mapply h/set-attr (result-attrs a)))))

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
