(ns adventofcode2023.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2023.core :refer :all]
            [clojure.zip :as z]))

(deftest day1-1-test
  (is (= (day1-1 "1-example.txt") 142))
  (is (= (day1-1 "1.txt") 55488)))

(deftest day1-2-test
  (is (= (day1-2 "1-2-example.txt") 281))
  (is (= (day1-2 "1.txt") 55614)))

(deftest day2-1-test
  (is (= (day2-1 "2-example.txt") 8))
  (is (= (day2-1 "2.txt") 2447)))

(deftest day2-2-test
  (is (= (day2-2 "2-example.txt") 2286))
  (is (= (day2-2 "2.txt") 56322)))

(deftest day3-1-test
  (is (= (day3-1 "3-example.txt") 4361))
  (is (= (day3-1 "3.txt") 531932)))

(deftest day3-2-test
  (is (= (day3-2 "3-example.txt") 467835))
  (is (= (day3-2 "3.txt") 73646890)))

(deftest day4-1-test
  (is (= (day4-1 "4-example.txt") 13))
  (is (= (day4-1 "4.txt") 21959)))

(deftest day4-2-test
  (is (= (day4-2 "4-example.txt") 30))
  (is (= (day4-2 "4.txt") 5132675)))

(deftest day5-1-test
  (is (= (day5-1 "5-example.txt") 35))
  (is (= (day5-1 "5.txt") 462648396)))