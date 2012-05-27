(ns baduk.test.core
  (:use [baduk.core])
  (:use [clojure.test]))

(deftest board-properties
  (is (vector? (board 3)))
  (is (= 9 (count (board 3))))
  (is (= 3 (board-size (board 3))))
  (is (= 1 (count (distinct (board 3)))))
  (is (= \. (first (distinct (board 3))))))

(deftest edges-checking
  (testing "3x3 board"
    (are [pos] (true? (is-edge? (board 3) pos)) 0 1 2 3 5 6 7 8)
    (are [pos] (false? (is-edge? (board 3) pos)) 4))
  (testing "4x4 board"
    (are [pos] (true? (is-edge? (board 4) pos)) 0 1 2 3 4 7 8 11 12 13 14 15)
    (are [pos] (false? (is-edge? (board 4) pos)) 5 6 9 10)))
