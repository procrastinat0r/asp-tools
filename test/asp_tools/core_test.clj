(ns asp-tools.core-test
  (:require [clojure.test :refer :all]
            [asp-tools.core :refer :all]))

(deftest dzn-to-lp-empty-atomic-constraint
  (testing "Convert empty Atomic Constraint from DZN to LP format"
    (is (= (format "%% atomic constraints\n")
           (dzn-to-lp-atomic_constraint "AtomicConstraints =  [ ] ;")))))

(deftest dzn-to-lp-atomic-constraint
  (testing "Convert Atomic Constraint from DZN to LP format"
     (let [dzn-str "AtomicConstraints =  [|
                                           14, 16|];
                                         "]
       (is (= (format "%% atomic constraints\natomiccon(14,16).\n")
              (dzn-to-lp-atomic_constraint dzn-str))))))

(deftest dzn-to-lp-2-atomic-constraints
  (testing "Convert multiple Atomic Constraints from DZN to LP format"
     (let [dzn-str "AtomicConstraints =  [|
                                           4, 9|
                                           14, 16|];
                                         "]
       (is (= (format "%% atomic constraints\natomiccon(4,9).\natomiccon(14,16).\n")
              (dzn-to-lp-atomic_constraint dzn-str))))))

(deftest dzn-to-lp-empty-disjunctive-constraint
  (testing "Convert empty Disjunctive Constraint from DZN to LP format"
    (is (= (format "%% disjunctive constraints\n")
           (dzn-to-lp-disjunctive-constraint "DisjunctiveConstraints =  [ ] ;")))))

(deftest dzn-to-lp-1-disjunctive-constraint
  (testing "Convert Disjunctive Constraint from DZN to LP format"
    (let [dzn-str "DisjunctiveConstraints =  [|4, 3, 4, 5|];"
          ]
      (is (= (format "%% disjunctive constraints\ndiscon(4,3,4,5).\n")
             (dzn-to-lp-disjunctive-constraint dzn-str))))))

(deftest dzn-to-lp-3-disjunctive-constraint
  (testing "Convert Disjunctive Constraint from DZN to LP format"
    (let [dzn-str "DisjunctiveConstraints =  [|4, 3, 4, 5|
                                                13, 2, 13, 11|
                                                13, 7, 13, 16|];
                                                "
          ]
      (is (= (format "%% disjunctive constraints\ndiscon(4,3,4,5).\ndiscon(13,2,13,11).\ndiscon(13,7,13,16).\n")
             (dzn-to-lp-disjunctive-constraint dzn-str))))))
