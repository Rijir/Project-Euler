(ns project-euler.core-test
  (:require [clojure.test :refer [deftest is]]
            [project-euler.core :as core]))

(deftest problem1-test
  (is (= (core/problem1) 233168)))
