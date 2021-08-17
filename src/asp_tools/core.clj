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

(defn dzn-to-lp-atomic-constraint
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

(defn dzn-to-lp-direct-successor-constraint
  "Convert a Direct Successor Constraint from DZN to LP format"
  [dzn-str]
  (dzn-to-lp-common-atomic-constraint dzn-str "DirectSuccessors" "direct successor" "???"))

(defn- dzn-to-lp-get-section
  "Get a section from a DZN problem"
  [dzn-str tag]
  (let [pat (re-pattern (format "(?s).*?(\\b%s\\s+=\\s+.*?;).*" tag))
        [_ section](re-matches pat dzn-str)
        ]
    (if section section "")))

(defn dzn-to-lp-k
  "Convert K spec from DZN format to LP format"
  [dzn-str]
  (let [[_ k] (re-matches #"(?s)\s*k\s+=\s+(\d+)\s*;" dzn-str)]
    (if k (format "%% num of cables\n#const k=%s.\n" k) "")))

(defn dzn-to-lp-b
  "Convert B spec from DZN format to LP format"
  [dzn-str]
  (let [[_ b] (re-matches #"(?s)\s*b\s+=\s+(\d+)\s*;" dzn-str)]
    (if b (format "%% num of 2-sided cables\n#const b=%s.\n" b) "")))

(defn dzn-to-lp
  "Convert a DZN problem to LP format"
  [prb-name dzn-str]
  (str/join [
             (format "%% benchmark %s\n" prb-name)
             (dzn-to-lp-k (dzn-to-lp-get-section dzn-str "k"))
             (dzn-to-lp-b (dzn-to-lp-get-section dzn-str "b"))
             (format "\n")
             (dzn-to-lp-atomic-constraint (dzn-to-lp-get-section dzn-str "AtomicConstraints"))
             (format "\n")
             (dzn-to-lp-disjunctive-constraint (dzn-to-lp-get-section dzn-str "DisjunctiveConstraints"))
             (format "\n")
             (dzn-to-lp-soft-atomic-constraint (dzn-to-lp-get-section dzn-str "SoftAtomicConstraints"))
             (format "\n")
             (dzn-to-lp-direct-successor-constraint (dzn-to-lp-get-section dzn-str "DirectSuccessors"))
             (format "\n")
             ]))
