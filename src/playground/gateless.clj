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

(defn check-name [acc [word1 word2]]
  (let [firstname (= :firstname (name-check word1))
        lastname (= :lastname (name-check word2))]
    (cond
      (and firstname lastname) (conj acc (str word1 " " word2))
      (and firstname (false? lastname)) (conj acc word1)
      :else acc))) ;; {:firstname "STeve" :lastname "Triphahn"}

(defn extract-names [article-text]
  (->> article-text
       (re-seq #"\w+")
       (partition-all 2 1)
       (reduce check-name [])))

(comment
  (def article "I am an article for Kyoshi. This is John Smith.  This is Steve Triphahn.\nThis is another sentence.")
  (extract-names article) ;; => ["John Smith" "Steve Triphahn"]
  (re-seq #"\w+" article)
  #_())
