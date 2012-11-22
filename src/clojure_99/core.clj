(ns clojure-99.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 99 Clojure Problems ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lists

(defn p01 [lst]
  "Find the last element of a list."
  (last lst))

(defn p02 [lst]
  "Find the last but one element of a list."
  (last (butlast lst)))

(defn p03 [lst k]
  "Find the Kth element of a list."
  (nth lst (dec k)))

(defn p04 [lst]
  "Find the number of elements of a list."
  (count lst))

(defn p05 [lst]
  "Reverse a list."
  (reverse lst))

(defn p06 [lst]
  "Find out whether a list is a palindrome."
  (= (reverse lst) lst))

(defn p07 [lst]
  "Flatten a nested list structure."
  (flatten lst))

(defn p08 [lst]
  "Eliminate consecutive duplicates of list elements."
  (map first (partition-by identity lst)))

(defn p09 [lst]
  "Pack consecutive duplicates of list elements into sublists."
  (partition-by identity lst))

(defn p10 [lst]
  "Run-length encoding of a list."
  (map (juxt count first) (p09 lst)))

(defn p11 [lst]
  "Modified run-length encoding."
  (map #(if (= 1 (first %)) (second %) %) (p10 lst)))

(defn p12 [lst]
  "Decode a run-length encoded list."
  (flatten (map #(if (coll? %) (repeat (first %) (second %)) %) lst)))

(defn p13 [lst]
  "Run-length encoding of a list (direct solution)."
  (loop [[h & t] lst prev (first lst) c 0 acc []]
    (if h (if (= prev h)
            (recur t prev (inc c) acc)
            (recur t h 1 (if (= c 1) (conj acc prev) (conj acc [c prev]))))
        (if (= c 1) (conj acc prev) (conj acc [c prev])))))

(defn p14 [lst]
  "Duplicate the elements of a list."
  (interleave lst lst))

(defn p15 [lst k]
  "Duplicate the elements of a list a given number of times."
  (mapcat #(repeat k %) lst))

(defn p16 [lst n]
  "Drop every Nth element from a list."
  (flatten (partition-all (dec n) n lst)))

(defn p17 [lst n]
  "Split a list into two parts; the length of the first part is given."
  (split-at n lst))

(defn p18 [lst n k]
  "Extract a slice from a list."
  (take (inc (- k n)) (drop (dec n) lst)))

(defn p19 [lst n]
  "Rotate a list N places to the left."
  (let [k (if (pos? n) n (+ n (count lst)))
        [a b] (split-at k lst)]
    (concat b a)))

(defn p20 [lst k]
  "Remove the Kth element from a list."
  (concat (take (dec k) lst) (drop k lst)))