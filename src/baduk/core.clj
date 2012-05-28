(ns baduk.core)

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
  [board pos stone]
  (assoc board pos stone))

(defn white?
  [board pos]
  (= white (board pos)))

(defn black?
  [board pos]
  (= black (board pos)))

(defn no-stone?
  [board pos]
  (= no-stone (board pos)))

(defn print-board
  [board]
  (let [size (board-size board)
        rows (partition size board)]
    (doseq [row rows] (println row))))
