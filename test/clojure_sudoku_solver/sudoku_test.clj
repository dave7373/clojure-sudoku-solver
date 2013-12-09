(ns clojure_sudoku_solver.sudoku_test
  (:require [clojure.test :refer :all]
            [clojure_sudoku_solver.sudoku :refer :all]))

(deftest box-test
  (testing "box function"
    (is (= 7 (box 77 9)))
    (is (= 2 (box 8 9)))
    (is (= 0 (box 0 9)))
    ))

(deftest col-test
  (testing "col function"
    (is (= 5 (col 77 9)))
    (is (= 8 (col 8 9)))
    (is (= 0 (col 0 9)))
    ))

(deftest row-test
  (testing "row function"
    (is (= 8 (row 77 9)))
    (is (= 0 (row 8 9)))
    (is (= 0 (row 0 9)))
    ))

(deftest has-initial-value-test
  (testing "initial value detection"
    (let [sudoku1 (set-value-at-position empty-sudoku 0 1)
          sudoku2 (set-value-at-position sudoku1 80 9)]
      (is (has-initial-value sudoku1 0))
      (is (not (has-initial-value sudoku1 1)))
      (is (has-initial-value sudoku2 0))
      (is (not (has-initial-value sudoku2 1)))
      (is (has-initial-value sudoku2 80))
      (is (not (has-initial-value sudoku2 79)))
      )))

(def easy-sudoku-start
  '(0 7 0 0 0 9 6 2 5
    0 6 0 0 8 2 0 0 0
    0 0 1 0 0 0 8 0 9
    0 0 8 0 7 0 9 0 0
    0 2 9 0 0 0 7 6 8
    0 0 6 0 9 3 0 5 4
    0 8 0 9 0 0 1 7 0
    1 4 7 6 0 0 0 9 0
    5 0 3 1 0 0 0 8 6))

(deftest init-test
  (testing "load a sudoku start"
    (let [sudoku-start (init-sudoku easy-sudoku-start)
          result (get-vals sudoku-start)]
      (is (= easy-sudoku-start result))
      )))

(deftest solv-test-1
  (testing "solve a sudoku"
    (let [result (solve easy-sudoku-start)]
      (println "Pretty-printed:")
      (println (print-line result))
      )))

