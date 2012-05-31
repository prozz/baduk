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
    (cond
     (< pos size) true
     (>= pos (- (* size size) size)) true
     (= 0 (mod (+ pos 1) size)) true
     (= 0 (mod pos size)) true
     :else false)))

(defn corner?
  [board pos]
  (let [size (board-size board)]
    (cond
     (= pos 0) true
     (= pos (- size 1)) true
     (= pos (- (* size size) 1)) true
     (= pos (- (* size size) size)) true
     :else false)))

(defn put-stone
  [board stone pos]
  (assoc board pos stone))

(defn put-stones
  [board stone & positions]
  ; syntatic sugar for:
  ; (reduce (fn [board pos] (put-stone board stone pos)) board positions))
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
  (cond
   (white? board pos) white
   (black? board pos) black
   :else no-stone))

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
        left (if (= 0 (mod pos size)) :invalid (- pos 1))
        right (if (= 0 (mod (+ pos 1) size)) :invalid (+ pos 1))
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
            (distinct checked)
            (recur
              (remove (into #{} checked) (flatten (map #(adjacent-positions board %1) group-stones)))
              (into group-stones checked))))))))

(defn count-liberties
  [board pos]
  (count
    (filter #(no-stone? board %1)
      (distinct (flatten (map #(adjacent-positions board %1) (group-positions board pos)))))))

(defn legal-move?
  [board stone pos]
  (if (not (no-stone? board pos))
    (throw (IllegalArgumentException. "illegal move, stone is there..."))
    (let [board (put-stone board stone pos)
          group (group-positions board pos)
          liberties (count-liberties board pos)
          pos-around (flatten (map #(adjacent-positions board %1) group))
          color? (get {black white? white black?} stone)
          opp-stones (filter #(color? board %1) pos-around)
          opp-liberties (map #(count-liberties board %1) opp-stones)
          opp-alive (count (filter #(> %1 0) opp-liberties))]
      (cond
        (> liberties 0) true
        (and
          (= 0 liberties)
          (empty? opp-stones)) false
        (and
          (= 0 liberties)
          (= 0 opp-alive)) true
        :else false))))
