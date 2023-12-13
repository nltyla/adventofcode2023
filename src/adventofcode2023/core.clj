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
  (let [v (str/split-lines (slurp (io/resource name)))
        xf (comp
             (map #(str/replace % #"[^0-9]" ""))
             (map #(parse-int (str (first %) (last %)))))]
    (transduce xf + v)))

(defn day1-1
  "--- Day 1: Trebuchet?! ---"
  [name]
  (let [v (inputs name identity)
        xf (comp
             (map #(str/replace % #"[^0-9]" ""))
             (map #(parse-int (str (first %) (last %)))))]
    (transduce xf + v)))

(defn map-numbers [pairs s]
  (let [first-digit (apply min-key #(or (str/index-of s (second %)) Integer/MAX_VALUE) pairs)
        last-digit (apply max-key #(or (str/last-index-of s (second %)) Integer/MIN_VALUE) pairs)]
    (parse-int (str (first first-digit) (first last-digit)))))

(defn day1-2
  "--- Day 1 Part Two: Trebuchet?! ---"
  [name]
  (let [v (inputs name identity)
        num-str-pairs (concat
                        (map-indexed vector (map str (range 10)))
                        (map-indexed vector ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]))
        xf (map #(map-numbers num-str-pairs %))]
    (transduce xf + v)))

(defn day2-1-parse-line [s]
  (let [[game hands] (str/split s #": ")
        game-nr (parse-int (second (str/split game #" ")))
        vhands (partition 2 (str/split hands #" |, |; "))
        pairs (map #(vector (keyword (second %)) (parse-int (first %1))) vhands)]
    [game-nr pairs]))

(def day1-2-thresholds {:red 12 :green 13 :blue 14})
(defn day2-1
  "--- Day 2: Cube Conundrum ---"
  [name]
  (let [v (inputs name day2-1-parse-line)
        okrows (filter
                 (fn [[_ pairs]]
                   (every?
                     (fn [[color cnt]] (<= cnt (color day1-2-thresholds)))
                     pairs)) v)
        oknr (map first okrows)]
    (reduce + oknr)))