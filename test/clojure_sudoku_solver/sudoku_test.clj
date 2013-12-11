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

(defn time-myfunc [func]
  (let [starttime (System/nanoTime)
        return (func)
        endtime (System/nanoTime)]
    (println "time elapsed"
             (/ (- endtime starttime) 1e9))
    return))

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

(def easy-sudoku-solution
  '(8 7 4 3 1 9 6 2 5
    9 6 5 4 8 2 3 1 7
    2 3 1 7 6 5 8 4 9
    4 5 8 2 7 6 9 3 1
    3 2 9 5 4 1 7 6 8
    7 1 6 8 9 3 2 5 4
    6 8 2 9 5 4 1 7 3
    1 4 7 6 3 8 5 9 2
    5 9 3 1 2 7 4 8 6))



(deftest init-test
  (testing "load a sudoku start"
    (let [sudoku-start (init-sudoku easy-sudoku-start)
          result (get-vals sudoku-start)]
      (is (= easy-sudoku-start result))
      )))

(deftest solv-test-1
  (testing "solve a sudoku"
    (let [result (time-myfunc (fn [] (solve easy-sudoku-start)))]
      (is (=  easy-sudoku-solution result))
      (println "Sudoku pretty-printed:")
      (println (print-line result))
      )))


;; This sudoku is especially time consuming to solve for
;; for sequential brute force algorithms.
(def hard-sudoku-start
  '(0,0,0,0,8,0,0,0,0,
    0,0,0,0,0,0,8,7,1,
    0,0,0,4,0,3,0,0,0,
    3,0,0,0,6,0,0,0,0,
    0,0,0,0,9,0,0,4,8,
    0,5,0,0,1,0,0,6,2,
    0,7,9,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,4,
    0,6,8,9,3,0,0,0,0))

(def hard-sudoku-solution
  '(6 9 2 1 8 7 4 3 5
    5 4 3 6 2 9 8 7 1
    8 1 7 4 5 3 2 9 6
    3 8 1 2 6 4 7 5 9
    7 2 6 3 9 5 1 4 8
    9 5 4 7 1 8 3 6 2
    1 7 9 5 4 2 6 8 3
    2 3 5 8 7 6 9 1 4
    4 6 8 9 3 1 5 2 7))

(deftest solv-test-2
  (testing "solve a very hard sudoku"
    (let [result (time-myfunc (fn [] (solve hard-sudoku-start)))]
      ;;(is (=  hard-sudoku-solution result))
      (println "Sudoku pretty-printed:")
      (println (print-line result))
      )))

