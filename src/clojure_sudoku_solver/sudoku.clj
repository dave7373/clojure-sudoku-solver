(ns clojure_sudoku_solver.sudoku)

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn start-map [size default-value]
  (zipmap (range 0 size) (repeat size default-value)))

(def empty-sudoku
  "Returns new empty Sudoku data structure with 9 rows and 9 columns"
  (let [n 9]
    {:n n
     :rows (start-map n #{})
     :cols (start-map n #{})
     :boxes (start-map n #{})
     :values (start-map (* n n) 0)}))

(def row (memoize quot))
(def col (memoize rem))
(defn- find-box [x n]
  (+ (* (row (row x n) 3) 3)
     (row (col x n) 3)))
(def box (memoize find-box))

(defn set-value-at-position
  [sudoku pos value]
  (let [n (:n sudoku)
        r (row pos n)
        c (col pos n)
        b (box pos n)
        row-data (get (:rows sudoku) r)
        col-data (get (:cols sudoku) c)
        box-data (get (:boxes sudoku) b)]
    (if (or (contains? row-data value)
            (contains? col-data value)
            (contains? box-data value))
      nil
      (if (= 0 value) ; TODO move handling of 0 out of here into init-sudoku
        (conj sudoku {:values (conj (:values sudoku) {pos value})})                      
        (conj sudoku {:rows (conj (:rows sudoku) {r (conj row-data value)})
                      :cols (conj (:cols sudoku) {c (conj col-data value)})
                      :boxes (conj (:boxes sudoku) {b (conj box-data value)})
                      :values (conj (:values sudoku) {pos value})
                      })))))

(defn build [sudoku pos val-list]
  (if (empty? val-list)
    sudoku
    (let [u-sudoku (set-value-at-position sudoku pos (first val-list))]
      (build u-sudoku (+ pos 1) (rest val-list)))))

(defn init-sudoku [list]
  (assert (= (* 9 9) (count list)))
  (build empty-sudoku 0 list))


(defn pretty-sorted
  [sudoku]
  {:values (sort-by first (:values sudoku))
   :rows (sort-by first (:rows sudoku))
   :cols (sort-by first (:cols sudoku))
   :boxes (sort-by first (:boxes sudoku))
   })


(defn print-line
  [list]
  (if (empty? list)
    "end"
    (let [two-lists (split-at 9 list)
          head (first two-lists)
          tail (second two-lists)]
      (println head)
      (print-line tail))))

(defn pretty-print-vals
  [sudoku]
  (let [keys-and-values (sort-by first (:values sudoku))
        values (vals keys-and-values)]
    (print-line values)))

(defn has-initial-value
  [sudoku pos]
  (let [value (get (:values sudoku) pos)]
    ;;(if (not= 0 value) (println "pos" pos " value" value "PRESET ######"))
    (not (= 0 value))))

(defn get-vals
  [sudoku]
  (vals (sort-by first (:values sudoku))))

(defn internal-solve
  [sudoku pos value]
  ;;(println "pos" pos " value" value "trying")
  (if (> pos 80)
    sudoku ; This is a solution
    (if (> value 9)
      nil ; This is a dead end
      (if (has-initial-value sudoku pos)
        (internal-solve sudoku (+ pos 1) 1)
        (let [result (set-value-at-position sudoku pos value)]
          (if (nil? result)
            (internal-solve sudoku, pos, (+ value 1))
            (let [deep-result (internal-solve result (+ pos 1) 1)]
              (if (nil? deep-result)
                (internal-solve sudoku pos (+ value 1))
                deep-result))))))))

(defn solve
  [list]
  (let [start (init-sudoku list)
        solution (internal-solve start 0 1)]
        ;; hhh (println "SOLUTION: " solution)
        ;; jjj (println "END OF SOLUTION")
    (if (nil? solution)
      "Can not solve this sudoku"
      (get-vals solution))))
