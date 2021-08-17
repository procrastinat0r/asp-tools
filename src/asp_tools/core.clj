(ns asp-tools.core
  (:require
            [clojure.string :as str]))

(defn dzn-to-lp-atomic_constraint
  "Convert an Atomic Constraint from DZN to LP format"
  [dzn-str]
  (let [
        [_ c](re-matches #"(?s)AtomicConstraints\s+=\s+\[\|(.*)\]\s*;.*" dzn-str)
        acs (when c (re-seq #"(\d+),\s+(\d+)\|" c))
        ]
    (str/join
     (concat
      [(println-str "% atomic constraints")]
      (for [[_ l r] acs]
        (format "atomiccon(%s,%s).\n" l r))))))

(defn dzn-to-lp-disjunctive-constraint
  "Convert an Disjunctive Constraint from DZN to LP format"
  [dzn-str]
  (let [
        [_ c](re-matches #"(?s)DisjunctiveConstraints\s+=\s+\[\|(.*)\]\s*;.*" dzn-str)
        dcs (when c (re-seq #"(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\|" c))
        ]
    (str/join
     (concat
      [(println-str "% disjunctive constraints")]
      (for [[_ v1 v2 v3 v4] dcs]
        (format "discon(%s,%s,%s,%s).\n" v1 v2 v3 v4))))))

(defn dzn-to-lp-soft-atomic-constraint
  "Convert a Soft Atomic Constraint from DZN to LP format"
  [dzn-str]
  (let [
        [_ c](re-matches #"(?s)SoftAtomicConstraints\s+=\s+\[\|(.*)\]\s*;.*" dzn-str)
        cs (when c (re-seq #"(\d+),\s+(\d+)\|" c))
        ]
    (str/join
     (concat
      [(println-str "% soft atomic constraints")]
      (for [[_ l r] cs]
        (format "softcon(%s,%s).\n" l r))))))
