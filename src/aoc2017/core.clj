(ns aoc2017.core)

(defn day1a
  {:test (fn []
    (clojure.test/is (= (day1a "1122") 3))
    (clojure.test/is (= (day1a "1111") 4))
    (clojure.test/is (= (day1a "1234") 0))
    (clojure.test/is (= (day1a "91212129") 9)))}
  [captcha]
  (reduce (fn [sum idx]
            (let [dig (-> (nth captcha idx)
                          (str)
                          (read-string))
                  nxtdig (-> (nth captcha (mod (inc idx) (count captcha)))
                             (str)
                             (read-string))]
                  (if (= dig nxtdig)
                      (+ sum dig)
                      sum)))
    0
    (range (count captcha))))

(comment (-> (slurp "data/input_day1.txt")
(clojure.string/trim)
(day1a))
;;1175
)

(defn day1b
  {:test (fn []
    (clojure.test/is (= (day1b "1212") 6))
    (clojure.test/is (= (day1b "1221") 0))
    (clojure.test/is (= (day1b "123425") 4))
    (clojure.test/is (= (day1b "123123") 12))
    (clojure.test/is (= (day1b "12131415") 4)))}
  [captcha]
  (reduce (fn [sum idx]
            (let [dig (-> (nth captcha idx)
                          (str)
                          (read-string))
                  nxtdig (-> (nth captcha (mod (+ idx (/ (count captcha) 2)) (count captcha)))
                             (str)
                             (read-string))]
                  (if (= dig nxtdig)
                      (+ sum dig)
                      sum)))
    0
    (range (count captcha))))

(comment (-> (slurp "data/input_day1.txt")
(clojure.string/trim)
(day1b))
;;1166
)
