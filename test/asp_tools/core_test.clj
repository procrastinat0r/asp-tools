(ns asp-tools.core-test
  (:require [clojure.test :refer :all]
            [asp-tools.core :refer :all]))

(deftest dzn-to-lp-empty-atomic-constraint
  (testing "Convert empty Atomic Constraint from DZN to LP format"
    (is (= (format "%% atomic constraints\n") (dzn-to-lp-atomic_constraint "AtomicConstraints =  [ ] ;")))))

(deftest dzn-to-lp-atomic-constraint
  (testing "Convert Atomic Constraint from DZN to LP format"
     (let [dzn-str "AtomicConstraints =  [|
                                           14, 16|];
                                         "]
       (is (= (format "%% atomic constraints\natomiccon(14,16).\n")
              (dzn-to-lp-atomic_constraint dzn-str))))))

