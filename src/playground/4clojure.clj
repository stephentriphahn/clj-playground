(ns playground.4clojure
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which returns true if the given sequence is a palindrome. Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
(defn palindrome?
  [x]
  (every? true? (map = x (reverse x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 49, Split a sequence
;; Difficulty: easy
;; Write a function which will split a sequence into two parts.

(defn my-split [n s]
  (vector (take n s) (drop n s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which takes a sequence consisting of items with different
;;; types and splits them up into a set of homogeneous sub-sequences.
;;; The internal order of each sub-sequence should be maintained, but
;;; the sub-sequences themselves can be returned in any order (this is why 'set'
;;; is used in the test cases)

(defn split-by-type
  [coll]
  (->> coll (group-by type) vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Write a higher-order function which flips the order of the arguments of an
;;;  input function
(def flip #(fn [& args] (apply % (reverse args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which removes the duplicates from a sequence. Order of the
;;; items must be maintained.

(defn my-distinct
  [coll]
  (reduce
   (fn [acc x]
     (if (some #{x} acc)
       acc
       (conj acc x)))
   []
   coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which returns the first X fibonacci numbers.
(defn lazy-fibs
  ([] (lazy-fibs 1 1))
  ([a b]
   (lazy-seq (cons a (lazy-fibs b (+ a b))))))

(defn x-fibonacci
  [n]
  (take n (lazy-fibs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which reverses the interleave process into x number of subsequences.
(defn reverse-interleave
  [s n]
  (apply map list (partition-all n s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which takes lower-case hyphen-separated strings and converts
;;; them to camel-case strings.
(defn camel-case
  [s]
  (let [[words & r] (str/split s #"-")]
    (apply str words (map str/capitalize r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write a function which returns a sequence of lists of x items each. Lists of
;;; less than x items should not be returned.
(defn my-partition
  [n xs]
  (lazy-seq
   (let [chunk (take n xs)]
     (when (= n (count chunk))
       (cons chunk (my-partition n (drop n xs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implement comp
(defn my-comp [& fns]
  (reduce #(fn [& args]
             (% (apply %2 args))) fns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pasal's triangle lazy seq
(defn pascal-triangle
  [xs]
  (iterate
   #(let [s (concat [0] % [0])]
      (map +' s (next s)))
   xs))

(comment
  (#{1 2 3} 4)
  (partition 2 [1])
  (str (camel-case "foo-bar"))
  (my-distinct [1 2 1 3 4 3 2])
  (reverse-interleave [1 2 3 4 5 6] 2)
  (apply map list [[1 2 3] [4 5 6]])
  #_())
