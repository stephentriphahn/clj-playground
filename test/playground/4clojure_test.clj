(ns playground.4clojure-test
  (:require [playground.4clojure :as sut]
            [clojure.test :refer [testing deftest is]]))
(deftest palindrome?-test
  (testing "returns true if sequence is a plaindrome"
    (is (false? (sut/palindrome? '(1 2 3 4 5))))
    (is (true? (sut/palindrome? "racecar")))
    (is (true? (sut/palindrome? [:foo :bar :foo])))
    (is (true? (sut/palindrome? '(1 1 3 3 1 1))))
    (is (false? (sut/palindrome? '(:a :b :c))))))
(deftest my-split-test
  (testing "splits into two parts"
    (is (= (sut/my-split 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
    (is (= (sut/my-split 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
    (is (= (sut/my-split 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

(deftest split-by-type-test
  (testing "splits by type"
    (is (= (set (sut/split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
    (is (= (set (sut/split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
    (is (= (set (sut/split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))))

(deftest flip-test
  (testing "returns a function with flipped arg order"
    (is (= 3 ((sut/flip nth) 2 [1 2 3 4 5])))
    (is (= true ((sut/flip >) 7 8)))
    (is (= 4 ((sut/flip quot) 2 8)))
    (is (= [1 2 3] ((sut/flip take) [1 2 3 4 5] 3)))))

(deftest my-distinct-test
  (testing "my-distinct dedupes items"
    (is (= (sut/my-distinct [1 2 1 3 1 2 4]) [1 2 3 4]))
    (is (= (sut/my-distinct [:a :a :b :b :c :c]) [:a :b :c]))
    (is (= (sut/my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
    (is (= (sut/my-distinct (range 50)) (range 50)))))

(deftest x-fibonacci-test
  (testing "returns first X fibonacci numbers"
    (is (= (sut/x-fibonacci 3) '(1 1 2)))
    (is (= (sut/x-fibonacci 6) '(1 1 2 3 5 8)))
    (is (= (sut/x-fibonacci 8) '(1 1 2 3 5 8 13 21)))))

(deftest reverse-interleave-test
  (testing "reverses the interleave process into x number of subsequences"
    (is (= (sut/reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
    (is (= (sut/reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
    (is (= (sut/reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(deftest reverse-interleave-test
  (testing "reverses the interleave process into n subsequences"
    (is (= (sut/reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
    (is (= (sut/reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
    (is (= (sut/reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(deftest camel-case-test
  (testing "camelCases hyphen seperated strings"
    (is (= (sut/camel-case "something") "something"))
    (is (= (sut/camel-case "multi-word-key") "multiWordKey"))
    (is (= (sut/camel-case "leaveMeAlone") "leaveMeAlone"))))

(deftest my-partition-test
  (testing "returns a sequence of lists of x items each"
    (is (= (sut/my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
    (is (= (sut/my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
    (is (= (sut/my-partition 3 (range 8)) '((0 1 2) (3 4 5))))
    (is (= '() (sut/my-partition 2 '(1))))))

(deftest my-comp-test
  (testing "composes functions right to left"
    (is (= [3 2 1] ((sut/my-comp rest reverse) [1 2 3 4])))
    (is (= 5 ((sut/my-comp (partial + 3) second) [1 2 3 4])))
    (is (= true ((sut/my-comp zero? #(mod % 8) +) 3 5 7 9)))
    (is (= "HELLO" ((sut/my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")))))

(deftest pascal-triangle-test
  (testing "pascals triangle"
    (is (= (second (sut/pascal-triangle [2 3 2])) [2 5 5 2]))
    (is (= (take 5 (sut/pascal-triangle [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
    (is (= (take 2 (sut/pascal-triangle [3 1 2])) [[3 1 2] [3 4 3 2]]))
    (is (= (take 100 (sut/pascal-triangle [2 4 2])) (rest (take 101 (sut/pascal-triangle [2 2])))))))
