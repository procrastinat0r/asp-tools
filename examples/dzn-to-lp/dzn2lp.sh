#!/usr/bin/env clj

(ns asp-tools.dzn2lp
 (:require [asp-tools.core :refer :all]))

(defn- usage []
   (println)
   (println "dzn2lp <prb-name> <dzn-file> <lp-file>")
   (println)
   (println "  convert <dzn-file> in DZN format to <lp-file> in LP format using <prb-name> as problem name")
   (println))

(let [args *command-line-args*
      [prb-name dzn-file lp-file] args]
   (if (= 3 (count args))
       (dzn-to-lp-convert-file prb-name dzn-file lp-file)
       (usage)))
