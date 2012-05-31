(ns baduk.test.core
  (:use [baduk.core])
  (:use [clojure.test]))

(defmacro testing-on
  "pretty print of board under test for easy debugging"
  [board & body]
  `(let [s# (board-size ~board)]
     (testing (str "on " s# "x" s# " board like:\n" (print-board-str ~board)) ~@body)))

(deftest board-properties
  (let [board (board 3)]
    (is (vector? board))
    (is (= 9 (count board)))
    (is (= 3 (board-size board)))
    (is (= 1 (count (distinct board))))
    (is (= \. (first (distinct board))))))

(deftest edges-checking
  (let [board (board 3)]
    (testing-on board
      (are [pos] (true? (edge? board pos)) 0 1 2 3 5 6 7 8)
      (are [pos] (false? (edge? board pos)) 4)))
  (let [board (board 4)]
    (testing-on board
      (are [pos] (true? (edge? board pos)) 0 1 2 3 4 7 8 11 12 13 14 15)
      (are [pos] (false? (edge? board pos)) 5 6 9 10))))

(deftest corner-checking
  (let [board (board 3)]
    (testing-on board
      (are [pos] (true? (corner? board pos)) 0 2 6 8)
      (are [pos] (false? (corner? board pos)) 1 3 4 5 7)))
  (let [board (board 4)]
    (testing-on board
      (are [pos] (true? (corner? board pos)) 0 3 12 15)
      (are [pos] (false? (corner? board pos)) 1 2 4 5 6 7 8 9 10 11 13 14))))

(deftest board-examining
  (let [board (board 3)]
    (testing-on board
      (testing "no stones"
        (are [pos] (true? (no-stone? board pos)) 0 1 2 3 4 5 6 7 8))
      (testing "single white stone at random position"
        (let [pos (rand-int 9)
              board (put-stone board white pos)]
          (is (white? board pos))))
      (testing "single black stone at random position"
        (let [pos (rand-int 9)
              board (put-stone board black pos)]
          (is (black? board pos)))))))

(deftest adjacent-positions-checking
  (let [board (board 3)]
    (testing "corner"
      (is (= '(1 3) (sort (adjacent-positions board 0))))
      (is (= '(1 5) (sort (adjacent-positions board 2))))
      (is (= '(3 7) (sort (adjacent-positions board 6))))
      (is (= '(5 7) (sort (adjacent-positions board 8)))))
    (testing "edge"
      (is (= '(0 2 4) (sort (adjacent-positions board 1))))
      (is (= '(0 4 6) (sort (adjacent-positions board 3))))
      (is (= '(2 4 8) (sort (adjacent-positions board 5))))
      (is (= '(4 6 8) (sort (adjacent-positions board 7)))))
    (testing "middle"
      (is (= '(1 3 5 7) (sort (adjacent-positions board 4)))))))

(deftest group-positions-checking
  (testing "no stone"
    (let [board (board 3)]
      (is (thrown? IllegalArgumentException (group-positions board 4)))))
  (testing "single black stone"
    (let [board (put-stone (board 3) black 4)]
      (testing-on board
        (is (= '(4) (group-positions board 4))))))
  (testing "two black stones"
    (let [board (put-stones (board 3) black 4 1)]
      (testing-on board
        (are [pos] (= '(1 4) (sort (group-positions board pos))) 1 4))))
  (testing "single white stone"
    (let [board (put-stone (board 3) white 4)]
      (testing-on board
        (is (= '(4) (group-positions board 4))))))
  (testing "two white stones"
    (let [board (put-stones (board 3) white 4 1)]
      (testing-on board
        (are [pos] (= '(1 4) (sort (group-positions board pos))) 4 1))))
  (testing "group of three stones"
    (let [board (put-stones (board 3) white 3 4 5)]
      (testing-on board
        (are [pos] (= '(3 4 5) (sort (group-positions board pos))) 3 4 5))))
  (testing "empty triangle group near edges"
    (let [board (put-stones (board 3) white 0 1 3)]
      (testing-on board
        (are [pos] (= '(0 1 3) (sort (group-positions board pos))) 0 1 3))))
  (testing "empty triangle group in the middle"
    (let [board (put-stones (board 4) black 5 6 9)]
      (testing-on board
        (are [pos] (= '(5 6 9) (sort (group-positions board pos))) 5 6 9)))))

(deftest liberties-counting
  (testing "no stone"
    (let [board (board 3)]
      (are [pos] (thrown? IllegalArgumentException (count-liberties board pos)) 0 1 2 3 4 5 6 7 8)))
  (testing "single stone"
    (let [board (board 3)]
      (testing "in corner"
        (are [pos] (= 2 (count-liberties (put-stone board white pos) pos)) 0 2 6 8))
      (testing "at edge"
        (are [pos] (= 3 (count-liberties (put-stone board black pos) pos)) 1 3 5 7))
      (testing "in the middle"
        (is (= 4 (count-liberties (put-stone board black 4) 4))))))
  (testing "group with common liberties"
    (let [board (put-stones (board 3) white 1 3 4 5 7)]
      (testing-on board
        (are [pos] (= 4 (count-liberties board pos)) 1 3 4 5 7))))
  (testing "white group with black stones attached"
    (let [board (put-stones (put-stones (board 4) black 13 14) white 5 6 9 10)]
      (testing-on board
        (are [pos] (= 6 (count-liberties board pos)) 5 6 9 10)))))
