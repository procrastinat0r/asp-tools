(ns asp-tools.core-test
  (:require [clojure.test :refer :all]
            [asp-tools.core :refer :all]))

(deftest dzn-to-lp-empty-atomic-constraint
  (testing "Convert empty Atomic Constraint from DZN to LP format"
    (is (= "" (dzn-to-lp-atomic_constraint "AtomicConstraints =  [ ] ;")))))
