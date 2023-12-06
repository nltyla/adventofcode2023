(ns adventofcode2023.core-test
    (:require [clojure.test :refer :all]
      [adventofcode2023.core :refer :all]
      [clojure.zip :as z]))

(deftest day1-1-test
         (is (= (day1-1 "1-example.txt") 142)))