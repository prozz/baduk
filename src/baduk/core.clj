(ns baduk.core)

(defn board
  [size]
  (vec (repeat (* size size) \.)))

(defn board-size
  [board]
  (int (Math/sqrt (count board))))

(defn is-edge?
  [board pos]
  (let [size (board-size board)]
    (cond
     (< pos size) true
     (>= pos (- (* size size) size)) true
     (= 0 (mod (+ pos 1) size)) true
     (= 0 (mod pos size)) true
     :else false)))

(defn is-corner?
  [board pos]
  (let [size (board-size board)]
    (cond
     (= pos 0) true
     (= pos (- size 1)) true
     (= pos (- (* size size) 1)) true
     (= pos (- (* size size) size)) true
     :else false)))

(defn count-liberties
  [board pos]
  (cond
   (is-corner? board pos) 2
   (is-edge? board pos) 3
   :else 4))