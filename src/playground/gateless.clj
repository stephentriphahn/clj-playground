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

(defn check-name [word]
  (when-let [name-type (name-check word)]
    {name-type word})) ;; {:firstname "STeve" :lastname "Triphahn"}

(defn get-name
  [{:keys [firstname]} {:keys [lastname]}]
  (cond
    (and firstname lastname) (str firstname " " lastname)
    (and firstname (nil? lastname)) firstname
    :else nil))

;; return a list of names
(defn extract-names [article-text]
  (let [ns (map check-name (clojure.string/split article-text #"\W"))]
    (keep identity (map get-name ns (rest ns)))))

(comment
  (def article "I am an article for Kyoshi. This is John Smith.  This is Steve Triphahn.\nThis is another sentence.")
  (extract-names article) ;; => ["John Smith" "Steve Triphahn"]
  #_())
