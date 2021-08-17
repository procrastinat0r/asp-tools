(ns asp-tools.core
  (:require
            [clojure.string :as str]))

(defn- dzn-to-lp-common-atomic-constraint
  "Convert Soft or Atomic Constraints from DZN to LP format"
  [dzn-str dzn-tag lp-tag rule-name]
  (let [pat (re-pattern (format "(?s)%sConstraints\\s+=\\s+\\[\\|(.*)\\]\\s*;.*" dzn-tag))
        [_ c](re-matches pat dzn-str)
        cs (when c (re-seq #"(\d+),\s+(\d+)\|" c))
        ]
    (str/join
     (concat
      [(format "%% %s constraints\n" lp-tag)]
      (for [[_ l r] cs]
        (format "%scon(%s,%s).\n" rule-name l r))))))

(defn dzn-to-lp-atomic_constraint
  "Convert an Atomic Constraint from DZN to LP format"
  [dzn-str]
  (dzn-to-lp-common-atomic-constraint dzn-str "Atomic" "atomic" "atomic"))

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
  (dzn-to-lp-common-atomic-constraint dzn-str "SoftAtomic" "soft atomic" "soft"))
