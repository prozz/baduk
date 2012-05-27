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