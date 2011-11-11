(ns seqs.test.core
  (:use [seqs.core])
  (:use [clojure.test]))

(deftest normalizer
  (is (= ((make-result coll?) [1 2]) true))
  (is (= ((make-result seq) nil) false))
  (is (= ((make-result seq) 5) :e)))

(deftest runner
  (is (= (run-all ['inc 'coll?] ["nil" "(+ 2 3)"])
         {"inc" [:e true]
          "coll?" [false false]})))
