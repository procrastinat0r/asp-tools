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
