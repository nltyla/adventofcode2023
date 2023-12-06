(ns adventofcode2023.core
    (:require [clojure.java.io :as io]
      [clojure.string :as str]
      [clojure.set :as set]
      [clojure.data.priority-map :refer [priority-map]]
      [clojure.walk :as w]
      [clojure.zip :as z]
      [clojure.math.combinatorics :as combo]
      [clojure.pprint :as pp]))

(defn inputs
      "read file from resources, apply f to each line, return seq"
      [name f]
      (map f (str/split-lines (slurp (io/resource name)))))

(defn parse-int
      [s]
      (Integer/parseInt s))

(defn parse-ints-csv
      [s]
      (map parse-int (str/split s #",")))

(defn day1-1
  "--- Day 1: Trebuchet?! ---"
  [name]
  (let [v (inputs name identity)]
    (->> v
         (map #(str/replace % #"[^0-9]" ""))
         (map #(parse-int (str (first %) (last %))))
         (apply +))))