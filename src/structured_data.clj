(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
     (Math/pow a a)))

(defn spiff [v]
  (if (< (count v) 3)
    "?"
    (let [a (get v 0)
          b (get v 2)]
      (+ a b))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[a][x]] rectangle]
    (- x a)))

(defn height [rectangle]
  (let [[[a b][x y]] rectangle]
    (- y b)))


(defn square? [rectangle]
  (let [[[a b][c d]] rectangle]
    (==(- c a)
       (- d b))))

(defn area [rectangle]
  (*(width rectangle)
    (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a b][c d]] rectangle
        x (get point 0)
        y (get point 1)]
    (and (>= x a)
         (>= y b)
         (<= x c)
         (<= y d))))

(defn contains-rectangle? [outer inner]
  (let [[[a b][c d]] inner
        [[e f][g h]] outer]
    (and (contains-point? (rectangle [e f][g h])
                          (point a b))
         (contains-point? (rectangle [e f][g h])
                          (point c d)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [author (:authors book)]
    (assoc book :authors(conj author new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (if (empty? collection)
    '()
    (cons (count(first collection))
          (element-lengths(rest collection)))))

(defn second-elements [collection]
    (if (empty? collection)
      '()
      (cons(get(first collection) 1)
           (second-elements(rest collection)))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(==(count(set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
    (assoc book :authors(set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))))

(defn author->string [author]
  (let [nm (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if birth
      (str nm" ("birth" - "death")")
      (str nm))))

(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        nm (authors->string (:authors book))]
    (str title", written by "nm)))

(defn books->string [books]
  (let [qty (cond
              (== (count books) 0) "No books"
              (== (count books) 1) "1 book. "
              :else (str (count books) " books. "))]
    (str qty(apply str(interpose ". "(map book->string books)))".")))

(defn books-by-author [author books]
  (filter (fn[x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [aut] (= name (:name aut))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let[aut (:authors book)
       result (map alive? aut)]
  (every? identity result)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
