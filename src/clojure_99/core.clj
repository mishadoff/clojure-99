(ns clojure-99.core
  (:require [clojure.contrib.combinatorics :as comb])
  (:require [clojure.set :as sets])
  (:require [clojure.contrib.lazy-seqs :as lazy])
  (:require [clojure.contrib.math :as math]))

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

(defn p21 [e lst k]
  "Insert an element at a given position into a list."
  (concat (take (dec k) lst) (list e) (drop (dec k) lst)))

(defn p22 [a b]
  "Create a list containing all integers within a given range."
  (range a (inc b)))

(defn p23 [lst k]
  "Extract a given number of randomly selected elements from a list."
  (take k (shuffle lst)))

(defn p24 [k n]
  "Lotto: Draw N different random numbers from the set 1..N"
  (take k (shuffle (range 1 (inc n)))))

(defn p25 [lst]
  "Generate a random permutation of the elements of a list."
  (shuffle lst))

(defn p26 [lst k]
  "Generate the combinations of K distinct objects chosen
   from the N elements of a list"
  (comb/combinations lst k))

(defn p27-a [st]
  "Group the elements of a set into disjoint subsets.
   Size of list is 9, all element are different,
   groups will consist 2, 3 and 4 elements"
  (for [fg (comb/combinations st 2)
        sg (comb/combinations (remove #(contains? (set fg) %) st) 3)
        :let [lg (remove #(contains? (sets/union (set sg) (set fg)) %) st)]]
    (map set [fg sg lg])))

(defn p27-b [st groups]
  "Group the elements of a set into disjoint subsets.
   All element are different, size of lst equal to sum of groups elems."
  (let [n (count groups)
        f-rec (fn [st groups]
                (cond (= 1 (count groups)) [[st]]
                      :else (let [[g1 & gs] groups
                                  fg (comb/combinations st g1)]
                              (for [f fg]
                                (map #(cons (set f) %)
                                     (p27-b (sets/difference st (set f)) gs))))))]
    (partition n n (flatten (f-rec st groups)))))

(defn p28-a [lst]
  "Sorting list of lists according to the length of sublist"
  (reverse (sort-by count lst)))

(defn p28-b [lst]
  "Sorting list of lists according to the frequency of sublist length"
  (let [freqs (frequencies (map count lst))]
    (sort-by #(get freqs (count %)) lst)))

;; Arithmetic

(defn p29 [n]
  "Determine whether a given integer number is prime."
  (= n (first (drop-while #(< % n) lazy/primes))))

(defn p30 [n]
  "Determine the prime factors of a given positive integer."
  (loop [[p & ps] lazy/primes num n fact []]
    (if (= 1 num) fact
        (if (zero? (mod num p)) (recur lazy/primes (/ num p) (conj fact p))
            (recur ps num fact)))))

(defn p31 [n]
  "Determine the prime factors of a given positive integer with their multiplicity."
  (map (juxt first count) (partition-by identity (p30 n))))

(defn p32 [a b]
  "A list of prime numbers in given range."
  (take-while #(<= % b) (drop-while #(< % a) lazy/primes)))

(defn p33 [n]
  "Find two primes that sum to given number. The number must be even."
  (let [pspace (take-while #(< % n) lazy/primes)]
    (first (for [i pspace j pspace :when (= (+ i j) n)]
             [i j]))))

(defn p34-a [a b]
  "Find list of all Goldbach compositions in current range."
  (for [i (range a (inc b)) :when (even? i)]
    (cons i (p33 i))))

(defn p34-b [a b limit]
  "Find list of all Goldbach compositions where each number greaterthan limit."
  (for [[q w e] (p34-a a b) :when (and (> w limit) (> e limit))]
    [q w e]))

(defn p35 [a b]
  "Find Greatest Common Divisor for a and b."
  (math/gcd a b))

(defn p36 [a b]
  "Determines if two numbers are coprime."
  (= 1 (p35 a b)))

(defn p37 [m]
  "Euler's totient function."
  (reduce + (for [r (range 1 m)]
              (if (p36 r m) 1 0))))

(defn p38 [m]
  "Euler's totient function. Algorithm 2."
  (reduce * (for [[p m] (p31 m)]
              (* (dec p) (math/expt p (dec m))))))

(defn p39 [m]
  "Comparison of two Euler's totient function eimplemantations. Side effect."
  (let []
    (println "1st implementation: p37 =>" (time (p37 m)))
    (println "2nd implementation: p38 =>" (time (p38 m)))))