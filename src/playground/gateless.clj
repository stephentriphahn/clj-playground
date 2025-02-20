(ns playground.gateless)

(defn name-check [text]
  ; returns :firstname, :lastname or nil
  ; John -> :firstname
  ; Smith -> :lastname
  ; something -> nil
  (case text
    ("Kyoshi" "Steve" "John") :firstname
    ("Smith" "Triphahn") :lastname
    nil))

(def first-name? #(= % :firstname))
(def last-name? #(= % :lastname))
(defn check-name2
  [acc [first-word next-word]]
  (let [[t1 n1] first-word
        [t2 n2] next-word]
    (cond
      (and (first-name? t1) (last-name? t2)) (conj acc (str n1 " " n2))
      (first-name? t1) (conj acc n1)
      (last-name? t2) (conj acc n2)
      :default acc)))

(defn- list-names
  [text]
  (->> text
       (re-seq #"\w+")
       (cons nil)
       (map (juxt name-check identity))
       (partition-all 2 1)
       (reduce check-name2 [])))

(comment
  (def article "I am an article for Kyoshi. This is John Smith.  This is Steve Triphahn.\nThis is another sentence.")
  (def article2 "Triphahn. The name Smith is a last name. Kyoshi and Steve do not have that name. The following family names are cool: Smith, Triphahn.")
  (list-names article2) ;; => ["John Smith" "Steve Triphahn"]
  #_())
