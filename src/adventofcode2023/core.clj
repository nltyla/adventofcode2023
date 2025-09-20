(ns adventofcode2023.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.walk :as w]
            [clojure.zip :as z]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn parse-ints-csv
  [s]
  (map parse-long (str/split s #",")))

(defn day1-1
  "--- Day 1: Trebuchet?! ---"
  [name]
  (let [v (str/split-lines (slurp (io/resource name)))
        xf (comp
             (map #(str/replace % #"[^0-9]" ""))
             (map #(parse-long (str (first %) (last %)))))]
    (transduce xf + v)))

(defn day1-1
  "--- Day 1: Trebuchet?! ---"
  [name]
  (let [v (inputs name identity)
        xf (comp
             (map #(str/replace % #"[^0-9]" ""))
             (map #(parse-long (str (first %) (last %)))))]
    (transduce xf + v)))

(defn map-numbers [pairs s]
  (let [first-digit (apply min-key #(or (str/index-of s (second %)) Integer/MAX_VALUE) pairs)
        last-digit (apply max-key #(or (str/last-index-of s (second %)) Integer/MIN_VALUE) pairs)]
    (parse-long (str (first first-digit) (first last-digit)))))

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
  (let [[_ hands] (str/split s #": ")
        vhands (partition 2 (str/split hands #" |, |; "))]
    (map #(vector (keyword (second %)) (parse-long (first %1))) vhands)))

(def day1-2-thresholds {:red 12 :green 13 :blue 14})

(defn day2-1
  "--- Day 2: Cube Conundrum ---"
  [name]
  (let [v (inputs name day2-1-parse-line)
        oknr (keep-indexed
               (fn [index pairs]
                 (if (every? (fn [[color cnt]] (<= cnt (color day1-2-thresholds))) pairs)
                   (inc index)))
               v)]
    (reduce + oknr)))

(defn day2-2-parse-line [s]
  (let [[_ hands] (str/split s #": ")
        vhands (partition 2 (str/split hands #" |, |; "))]
    (reduce (fn [acc v]
              (update acc
                      (keyword (second v))
                      (fnil max 0)
                      (parse-long (first v))))
            {}
            vhands)))

(defn day2-2
  "--- Day 2 Part Two: Cube Conundrum ---"
  [name]
  (let [v (inputs name day2-2-parse-line)
        power (map #(* (:red %) (:green %) (:blue %)) v)]
    (reduce + power)))

(defn get-nums [s]
  (let [z (->> s
               (partition-by #(^[char] Character/isDigit %))
               (map #(apply str %))
               (filter #(^[char] Character/isDigit (first %)))
               (reduce
                 (fn [acc val]
                   (let [from (if-let [x (last acc)]
                                (+ (count (x 0)) (x 1))
                                0)]
                     (conj acc [val (str/index-of s val from)])))
                 []))]
    [s z]))

(defn valid-number
  [raw-rows [num idx]]
  (let [rowlen (count (first raw-rows))
        start (max 0 (dec idx))
        end (min rowlen (+ (count num) (+ idx 1)))
        z (some #(re-find #"[^0-9.]" (subs % start end)) raw-rows)]
    z))


(defn get-valid-numbers
  [row-partition]
  (let [raw-rows (map first row-partition)
        z (reduce (fn [acc vpair] (if (valid-number raw-rows vpair)
                                    (conj acc (parse-long (first vpair)))
                                    acc))
                  []
                  (second (second row-partition)))]

    z))


(defn day3-1
  "--- Day 3: Gear Ratios ---"
  [name]
  (let [v (inputs name identity)
        filler (apply str (repeat (count (first v)) \.))]
    (->> (concat (list filler) v (list filler))
         (map get-nums)
         (partition 3 1)
         (map get-valid-numbers)
         (map #(reduce + %))
         (reduce +))))

(defn add-num
  [row digits start col nums stars]
  (if (empty? digits)
    [[] nil nums stars]
    (let [num (parse-long (apply str digits))
          nums' (reduce
                  (fn [nums col]
                    (assoc nums [row col] num))
                  nums
                  (range start col))]
      [[] nil nums' stars])))

(defn parse-row
  [s row]
  (let [[_ _ nums stars] (reduce-kv
                           (fn [[digits start nums stars] col c]
                             (if (^[char] Character/isDigit c)
                               [(conj digits c) (or start col) nums stars]
                               (let [stars' (if (= \* c) (conj stars [row col]) stars)]
                                 (add-num row digits start col nums stars'))))
                           [[] nil {} []]
                           (conj (vec s) \.))]
    [nums stars]))

(defn day3-2
  [name]
  (let [f (inputs name identity)
        [nums stars] (reduce-kv
                       (fn [[allnums allstars] row s]
                         (let [[nums stars] (parse-row s row)]
                           [(conj allnums nums) (into allstars stars)]))
                       [{} []]
                       (vec f))
        sum (->> (map
                   (fn [[row col]]
                     (reduce
                       (fn [acc pos] (if-let [num (nums pos)]
                                       (conj acc num)
                                       acc))
                       #{}
                       [[(dec row) (dec col)] [(dec row) col] [(dec row) (inc col)]
                        [row (dec col)] [row (inc col)]
                        [(inc row) (dec col)] [(inc row) col] [(inc row) (inc col)]]))
                   stars)
                 (filter #(= 2 (count %)))
                 (map #(apply * %))
                 (reduce +))
        ]
    sum
    ))

(defn parse-longs [s]
  (into #{} (map parse-long) (str/split (str/trim s) #"\s+")))

(defn parse-day4-1 [s] (let [[_ nums] (str/split s #":")
                             [wins haves] (str/split nums #"\|")]
                         [(parse-longs wins) (parse-longs haves)]))

(defn day4-1
  "--- Day 4: Scratchcards ---"
  [name]
  (->> (inputs name parse-day4-1)
       (map #(apply set/intersection %))
       (filter seq)
       (map #(bit-shift-left 1 (dec (count %))))
       (reduce +)))

(defn day4-2
  "--- Day 4 Part Two: Scratchcards ---"
  [name]
  (let [wins (->> (inputs name parse-day4-1)
                  (map #(apply set/intersection %))
                  (mapv count))
        counts (vec (repeat (count wins) 1))
        total (reduce-kv
                (fn [acc idx wins]
                  (let [fpluscount (partial + (acc idx))]
                    (reduce
                      (fn [acc iidx] (update acc iidx fpluscount))
                      acc
                      (range (inc idx) (+ idx wins 1)))))
                counts
                wins)]

    (reduce + total)))