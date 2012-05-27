(ns baduk.test.core
  (:use [baduk.core])
  (:use [clojure.test]))

(deftest board-properties
  (let [board (board 3)]
  (is (vector? board))
  (is (= 9 (count board)))
  (is (= 3 (board-size board)))
  (is (= 1 (count (distinct board))))
  (is (= \. (first (distinct board))))))

(deftest edges-checking
  (testing "3x3 board"
    (let [board (board 3)]
      (are [pos] (true? (is-edge? board pos)) 0 1 2 3 5 6 7 8)
      (are [pos] (false? (is-edge? board pos)) 4)))
  (testing "4x4 board"
    (let [board (board 4)]
      (are [pos] (true? (is-edge? board pos)) 0 1 2 3 4 7 8 11 12 13 14 15)
      (are [pos] (false? (is-edge? board pos)) 5 6 9 10))))

(deftest corner-checking
  (testing "3x3 board"
    (let [board (board 3)]
      (are [pos] (true? (is-corner? board pos)) 0 2 6 8)
      (are [pos] (false? (is-corner? board pos)) 1 3 4 5 7)))
  (testing "4x4 board"
    (let [board (board 4)]
      (are [pos] (true? (is-corner? board pos)) 0 3 12 15)
      (are [pos] (false? (is-corner? board pos)) 1 2 4 5 6 7 8 9 10 11 13 14))))


(deftest liberties-counting
  (testing "alone stones on 3x3"
    (let [board (board 3)]
      (are [pos] (= 2 (count-liberties board pos)) 0 2 6 8)
      (are [pos] (= 3 (count-liberties board pos)) 1 3 5 7)
      (is (= 4 (count-liberties board 4))))))