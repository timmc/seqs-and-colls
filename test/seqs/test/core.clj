(ns seqs.test.core
  (:use [seqs.core])
  (:use [clojure.test]))

(deftest normalizer
  (is (= ((resultify coll?) [1 2])
         {:t true :v true}))
  (let [{:keys [t v]} ((resultify seq) [1 2 3])]
    (is (= t true))
    (is (= v (seq '(1 2 3)))))
  (let [{:keys [t v]} ((resultify seq) 5)]
    (is (= t :e))
    (is (instance? Exception v))))

(deftest runner
  (let [res (cross-eval ['inc 'coll? "#(* 2 %)"] ["nil" "(+ 2 3)"])
        res (for [[k vs] res]
              [k (for [v vs]
                   (update-in v [:v]
                              #(if (instance? Exception %) :exc %)))])]
    (is (= (into {} res)
           {"inc" [{:t :e, :v :exc} {:t true, :v 6}]
            "coll?" [{:t false, :v false} {:t false, :v false}]
            "#(* 2 %)" [{:t :e, :v :exc} {:t true, :v 10}]}))))
