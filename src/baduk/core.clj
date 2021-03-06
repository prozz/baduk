(ns baduk.core
  (:use [clojure.string :only (join) :as s]))

(def black \b)
(def white \w)
(def no-stone \.)

(defn board
  [size]
  (vec (repeat (* size size) no-stone)))

(defn board-size
  [board]
  (int (Math/sqrt (count board))))

(defn edge?
  [board pos]
  (let [size (board-size board)]
    (or
     (< pos size)
     (>= pos (- (* size size) size))
     (zero? (mod (inc pos) size))
     (zero? (mod pos size)))))

(defn corner?
  [board pos]
  (let [size (board-size board)]
    (or
     (zero? pos)
     (= pos (dec size))
     (= pos (dec (* size size)))
     (= pos (- (* size size) size)))))

(defn put-stone
  [board stone pos]
  (assoc board pos stone))

(defn put-stones
  [board stone & positions]
  (reduce #(put-stone %1 stone %2) board positions))

(defn white?
  [board pos]
  (= white (board pos)))

(defn black?
  [board pos]
  (= black (board pos)))

(defn no-stone?
  [board pos]
  (= no-stone (board pos)))

(defn what-stone?
  [board pos]
  (board pos))

(defn print-board-str
  [board]
  (let [size (board-size board)
        rows (partition size board)]
    (s/join (map println-str rows))))

(defn print-board
  [board]
  (print (print-board-str board)))

(defn adjacent-positions
  [board pos]
  (let [size (board-size board)
        up (- pos size)
        down (+ pos size)
        left (if (zero? (mod pos size)) :invalid (dec pos))
        right (if (zero? (mod (inc pos) size)) :invalid (inc pos))
        valid-pos? #(and (integer? %1) (>= %1 0) (< %1 (* size size)))]
    (filter valid-pos? (list up down left right))))

(defn group-positions
  [board pos]
  (if (no-stone? board pos)
    (throw (IllegalArgumentException. "no stone..."))
    (let [stone (what-stone? board pos)
          color? (get {black black? white white?} stone)]
      (loop [adjacent (adjacent-positions board pos)
             checked (list pos)]
        (let [group-stones (filter #(color? board %1) adjacent)]
          (if (empty? group-stones)
            checked
            (recur
              (remove (set checked) (flatten (map #(adjacent-positions board %1) group-stones)))
              (distinct (into group-stones checked)))))))))

(defn count-liberties
  [board pos]
  (count
    (filter #(no-stone? board %1)
      (distinct (flatten (map #(adjacent-positions board %1) (group-positions board pos)))))))

(defn legal-move?
  [board stone pos]
  (if-not (no-stone? board pos)
    (throw (IllegalArgumentException. "illegal move, stone is there..."))
    (let [board (put-stone board stone pos)
          liberties (count-liberties board pos)]
      (if (pos? liberties)
        true
        (let [group (group-positions board pos)
              pos-around (flatten (map #(adjacent-positions board %1) group))
              color? (get {black white? white black?} stone)
              opp-stones (filter #(color? board %1) pos-around)]
          (if (and
                (zero? liberties)
                (empty? opp-stones))
            false
            (let [opp-liberties (map #(count-liberties board %1) opp-stones)
                  opp-alive (count (filter pos? opp-liberties))]
              (and
                (zero? liberties)
                (zero? opp-alive)))))))))
