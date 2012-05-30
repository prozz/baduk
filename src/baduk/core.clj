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
        ;; doubles will not pass thru valid-pos? test and for sure
        ;; no exception will be raised (like with putting same values
        ;; to set)
        left (if (= 0 (mod pos size)) -0.1 (- pos 1))
        right (if (= 0 (mod (+ pos 1) size)) -0.2 (+ pos 1))
        valid-pos? #(and (>= %1 0) (< %1 (* size size)))]
    ;; somehow filtering set returns sorted seq, so its easy in test
    ;; to do assertions. unsure if its safe to leave it as is or if
    ;; its some undefined side effect only.
    (filter valid-pos? #{ up down left right })))

(defn count-liberties
  [board pos]
  (cond
   (no-stone? board pos) (throw (IllegalArgumentException. "nothing to count here..."))
   (corner? board pos) 2
   (edge? board pos) 3
   :else 4))
