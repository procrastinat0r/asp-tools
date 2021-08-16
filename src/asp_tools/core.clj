(ns asp-tools.core
  (:require [clojure.string :as str]))

(defn dzn-to-lp-atomic_constraint
  "Convert an Atomic Constraint from DZN to LP format"
  [dzn-str]
  (str/join
   (concat
    [(println-str "% atomic constraints")]
    (for [[_ l r] (let [
                        [_ constraints] (re-matches #"(?s)AtomicConstraints\s+=\s+\[\|(\s+\d+,\s+\d+\|)*\]\s*;.*" dzn-str)
                        acs (when constraints (re-seq #"(\d+),\s+(\d+)\|" constraints))
                        ]
                    acs)

          ]
      (format "atomiccon(%s,%s).\n" l r)))))
