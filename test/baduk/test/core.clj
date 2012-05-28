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
      (are [pos] (true? (edge? board pos)) 0 1 2 3 5 6 7 8)
      (are [pos] (false? (edge? board pos)) 4)))
  (testing "4x4 board"
    (let [board (board 4)]
      (are [pos] (true? (edge? board pos)) 0 1 2 3 4 7 8 11 12 13 14 15)
      (are [pos] (false? (edge? board pos)) 5 6 9 10))))

(deftest corner-checking
  (testing "3x3 board"
    (let [board (board 3)]
      (are [pos] (true? (corner? board pos)) 0 2 6 8)
      (are [pos] (false? (corner? board pos)) 1 3 4 5 7)))
  (testing "4x4 board"
    (let [board (board 4)]
      (are [pos] (true? (corner? board pos)) 0 3 12 15)
      (are [pos] (false? (corner? board pos)) 1 2 4 5 6 7 8 9 10 11 13 14))))

(deftest board-examining
  (testing "3x3 board"
    (let [board (board 3)]
      (testing "all is empty"
        (are [pos] (true? (no-stone? board pos) 0 1 2 3 4 5 6 7 8)))
      (testing "single white stone at random position"
        (let [pos (rand-int 9)
              board (put-stone board white pos)]
          (is (white? board pos))))
      (testing "single black stone at random position"
        (let [pos (rand-int 9)
              board (put-stone board black pos)]
          (is (black? board pos)))))))

(deftest liberties-counting
  (testing "3x3 board with single colour stones only"
    (let [board (board 3)]
      (testing "no stones"
        (are [pos] (thrown? IllegalArgumentException (count-liberties board pos)) 0 1 2 3 4 5 6 7 8))
      (testing "1 stone in corner"
        (let [board (put-stones board white 0 2 6 8)]
          (are [pos] (= 2 (count-liberties board pos)) 0 2 6 8)))
      (testing "1 stone at edge"
        (let [board (put-stones board black 1 3 5 7)]
          (are [pos] (= 3 (count-liberties board pos)) 1 3 5 7))))))