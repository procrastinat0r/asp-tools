(ns asp-tools.core-test
  (:require [clojure.test :refer :all]
            [asp-tools.core :refer :all]
            [clojure.string :as str]))

(deftest dzn-to-lp-empty-atomic-constraint
  (testing "Convert empty Atomic Constraint from DZN to LP format"
    (is (= (format "%% atomic constraints\n")
           (dzn-to-lp-atomic-constraint "AtomicConstraints =  [ ] ;")))))

(deftest dzn-to-lp-1-atomic-constraint
  (testing "Convert Atomic Constraint from DZN to LP format"
     (let [dzn-str "AtomicConstraints =  [|
                                           14, 16|];
                                         "]
       (is (= (format "%% atomic constraints\natomiccon(14,16).\n")
              (dzn-to-lp-atomic-constraint dzn-str))))))

(deftest dzn-to-lp-2-atomic-constraints
  (testing "Convert multiple Atomic Constraints from DZN to LP format"
     (let [dzn-str "AtomicConstraints =  [|
                                           4, 9|
                                           14, 16|];
                                         "]
       (is (= (format "%% atomic constraints\natomiccon(4,9).\natomiccon(14,16).\n")
              (dzn-to-lp-atomic-constraint dzn-str))))))

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
  (testing "Convert multiple Disjunctive Constraints from DZN to LP format"
    (let [dzn-str "DisjunctiveConstraints =  [|4, 3, 4, 5|
                                                13, 2, 13, 11|
                                                13, 7, 13, 16|];
                                                "
          ]
      (is (= (format "%% disjunctive constraints\ndiscon(4,3,4,5).\ndiscon(13,2,13,11).\ndiscon(13,7,13,16).\n")
             (dzn-to-lp-disjunctive-constraint dzn-str))))))

(deftest dzn-to-lp-empty-soft-atomic-constraint
  (testing "Convert empty Soft Atomic Constraint from DZN to LP format"
    (is (= (format "%% soft atomic constraints\n")
           (dzn-to-lp-soft-atomic-constraint "SoftAtomicConstraints =  [ ] ;")))))

(deftest dzn-to-lp-1-soft-atomic-constraint
  (testing "Convert Soft Atomic Constraint from DZN to LP format"
     (let [dzn-str "SoftAtomicConstraints =  [|
                                           9, 1|];
                                         "]
       (is (= (format "%% soft atomic constraints\nsoftcon(9,1).\n")
              (dzn-to-lp-soft-atomic-constraint dzn-str))))))

(deftest dzn-to-lp-3-soft-atomic-constraints
  (testing "Convert multiple Soft Atomic Constraints from DZN to LP format"
     (let [dzn-str "SoftAtomicConstraints =  [|
                                           9, 1|
                                           1, 2|
                                           17, 18|];
                                         "]
       (is (= (format "%% soft atomic constraints\nsoftcon(9,1).\nsoftcon(1,2).\nsoftcon(17,18).\n")
              (dzn-to-lp-soft-atomic-constraint dzn-str))))))

(deftest dzn-to-lp-empty-direct-successors-constraints
  (testing "Convert empty Direct Successors Constraint from DZN to LP format"
    (is (= (format "%% direct successor constraints\n")
           (dzn-to-lp-direct-successor-constraint "DirectSuccessors =  [] ;")))))

(deftest dzn-to-lp-1-direct-successor-constraints
  (testing "Convert Direct Successor Constraint from DZN to LP format"
     (let [dzn-str "DirectSuccessors =  [|
                                           14, 16|];
                                         "]
       (is (= (format "%% direct successor constraints\ndirsuccon(14,16).\n")
              (dzn-to-lp-direct-successor-constraint dzn-str))))))

(deftest dzn-to-lp-2-direct-successor-constraints
  (testing "Convert multiple Direct Successor Constraints from DZN to LP format"
     (let [dzn-str "DirectSuccessors =  [|
                                           4, 9|
                                           14, 16|];
                                         "]
       (is (= (format "%% direct successor constraints\ndirsuccon(4,9).\ndirsuccon(14,16).\n")
              (dzn-to-lp-direct-successor-constraint dzn-str))))))


(deftest dzn-to-lp-empty
  (testing "Convert empty DZN content to LP"
    (let [dzn-str "
                                         "
          lp-str  (str/join [
                             (format "%% benchmark A031\n\n")
                             (format "%% atomic constraints\n\n")
                             (format "%% disjunctive constraints\n\n")
                             (format "%% soft atomic constraints\n\n")
                             (format "%% direct successor constraints\n\n")
                             ])
          ]
      (is (= lp-str
             (dzn-to-lp "A031" dzn-str))))))

(deftest dzn-to-lp-no-constraints
  (testing "Convert DZN problem without any constraints to LP"
    (let [dzn-str " k = 18;
                   b = 9;
                   "
          lp-str  (str/join [
                             (format "%% benchmark A031\n%% num of cables\n#const k=18.\n%% num of 2-sided cables\n#const b=9.\n\n")
                             (format "%% atomic constraints\n\n")
                             (format "%% disjunctive constraints\n\n")
                             (format "%% soft atomic constraints\n\n")
                             (format "%% direct successor constraints\n\n")
                             ])
          ]
      (is (= lp-str
             (dzn-to-lp "A031" dzn-str))))))

(deftest dzn-to-lp-with-atomic-constraints
  (testing "Convert DZN problem with only atomic constraints to LP"
    (let [dzn-str " k = 18;
                    b = 9;
                    AtomicConstraints =  [|
                    4, 8|
                    14, 11|
                    14, 16|];
                   "
          lp-str  (str/join [
                             (format "%% benchmark A031\n%% num of cables\n#const k=18.\n%% num of 2-sided cables\n#const b=9.\n\n")
                             (format "%% atomic constraints\natomiccon(4,8).\natomiccon(14,11).\natomiccon(14,16).\n\n")
                             (format "%% disjunctive constraints\n\n")
                             (format "%% soft atomic constraints\n\n")
                             (format "%% direct successor constraints\n\n")
                             ])
          ]
      (is (= lp-str
             (dzn-to-lp "A031" dzn-str))))))

(deftest dzn-to-lp-with-disjunctive-constraints
  (testing "Convert DZN problem with only disjunctive constraints to LP"
    (let [dzn-str " k = 18;
                    b = 9;
                    DisjunctiveConstraints =  [|4, 3, 4, 5|
                    13, 7, 13, 16|];
                    "
          lp-str  (str/join [
                             (format "%% benchmark A031\n%% num of cables\n#const k=18.\n%% num of 2-sided cables\n#const b=9.\n\n")
                             (format "%% atomic constraints\n\n")
                             (format "%% disjunctive constraints\ndiscon(4,3,4,5).\ndiscon(13,7,13,16).\n\n")
                             (format "%% soft atomic constraints\n\n")
                             (format "%% direct successor constraints\n\n")
                             ])
          ]
      (is (= lp-str
             (dzn-to-lp "A031" dzn-str))))))

(deftest dzn-to-lp-with-soft-atomic-constraints
  (testing "Convert DZN problem with only soft atomic constraints to LP"
    (let [dzn-str " k = 18;
                    b = 9;
                    SoftAtomicConstraints =  [|
                    9, 1|
                    17, 18|];
                    "
          lp-str  (str/join [
                             (format "%% benchmark A031\n%% num of cables\n#const k=18.\n%% num of 2-sided cables\n#const b=9.\n\n")
                             (format "%% atomic constraints\n\n")
                             (format "%% disjunctive constraints\n\n")
                             (format "%% soft atomic constraints\nsoftcon(9,1).\nsoftcon(17,18).\n\n")
                             (format "%% direct successor constraints\n\n")
                             ])
          ]
      (is (= lp-str
             (dzn-to-lp "A031" dzn-str))))))

(deftest dzn-to-lp-with-direct-successor-constraints
  (testing "Convert DZN problem with only direct successor constraints to LP"
    (let [dzn-str " k = 18;
                    b = 9;
                    DirectSuccessors =  [];
                    "
          lp-str  (str/join [
                             (format "%% benchmark A031\n%% num of cables\n#const k=18.\n%% num of 2-sided cables\n#const b=9.\n\n")
                             (format "%% atomic constraints\n\n")
                             (format "%% disjunctive constraints\n\n")
                             (format "%% soft atomic constraints\n\n")
                             (format "%% direct successor constraints\n\n")
                             ])
          ]
      (is (= lp-str
             (dzn-to-lp "A031" dzn-str))))))
