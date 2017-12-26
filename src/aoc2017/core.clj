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

(defn day2a-row
  {:test (fn []
    (clojure.test/is (= (day2a-row "5\t1\t9\t5") 8))
    (clojure.test/is (= (day2a-row "7\t5\t3") 4))
    (clojure.test/is (= (day2a-row "2\t4\t6\t8") 6)))}
  [row]
  (let [rowvector (map read-string (clojure.string/split row #"\t"))]
    (- (apply max rowvector) (apply min rowvector))
  ))

  (defn day2a
    {:test (fn []
      (clojure.test/is (= (day2a "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8") 18)))}
    [spreadsheet]
    (reduce (fn [checksum row] (+ checksum (day2a-row row)))
      0
      (clojure.string/split spreadsheet #"\n")))

(comment (-> (slurp "data/input_day2.txt")
;;(clojure.string/trim)
(day2a))
;;46402
)


(defn day2b-row
  {:test (fn []
    (clojure.test/is (= (day2b-row "5\t9\t2\t8") 4))
    (clojure.test/is (= (day2b-row "9\t4\t7\t3") 3))
    (clojure.test/is (= (day2b-row "3\t8\t6\t5") 2)))}
  [row]
  (let [rowvector (map read-string (clojure.string/split row #"\t"))]
    (reduce (fn [result idx]
              (+ result (reduce (fn [inner-result inner-idx]
                                  (+ inner-result (if (and (not= idx inner-idx)
                                           (= (mod (nth rowvector idx)
                                              (nth rowvector inner-idx)) 0))
                                      (/ (nth rowvector idx)
                                         (nth rowvector inner-idx))
                                      0)))
                                  0
                                  (range (count rowvector)))))
      0
      (range (count rowvector)))
  ))

  (defn day2b
    {:test (fn []
      (clojure.test/is (= (day2b "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5") 9)))}
    [spreadsheet]
    (reduce (fn [checksum row] (+ checksum (day2b-row row)))
      0
      (clojure.string/split spreadsheet #"\n")))

(comment (-> (slurp "data/input_day2.txt")
;;(clojure.string/trim)
(day2b))
;;265
)


(defn day4a
  {:test (fn []
    (clojure.test/is (day4a "aa bb cc dd ee"))
    (clojure.test/is (not (day4a "aa bb cc dd aa")))
    (clojure.test/is (day4a "aa bb cc dd aaa"))
)}
  [passphrase]
  (let [wordvector (clojure.string/split passphrase #" ")
        wordset (set wordvector)]
        (= (count wordset) (count wordvector))))

(comment (as-> (slurp "data/input_day4.txt") $
;;(clojure.string/trim)
(clojure.string/split $ #"\n")
(filter day4a $)
(count $))
;;451
)

(defn day4b
  {:test (fn []
    (clojure.test/is (day4b "abcde fghij"))
    (clojure.test/is (not (day4b "abcde xyz ecdab")))
    (clojure.test/is (day4b "a ab abc abd abf abj"))
    (clojure.test/is (day4b "iiii oiii ooii oooi oooo"))
    (clojure.test/is (not (day4b "oiii ioii iioi iiio")))
)}
  [passphrase]
  (let [wordvector (clojure.string/split passphrase #" ")
        sorted-wordvector (map sort wordvector)
        wordset (set sorted-wordvector)]
        (= (count wordset) (count wordvector))))

(comment (as-> (slurp "data/input_day4.txt") $
;;(clojure.string/trim)
(clojure.string/split $ #"\n")
(filter day4b $)
(count $))
;;223
)


(defn day5a
  {:test (fn []
           (clojure.test/is (= (day5a [0 3 0 1 -3]) 5)))
  }
  [offsets]
  (loop [offsets offsets idx 0 steps 0]
    (if (< -1 idx (count offsets))
        (recur (update offsets idx inc)
               (+ idx (nth offsets idx))
               (inc steps))
        steps))
)

(comment (as-> (slurp "data/input_day5.txt") $
;;(clojure.string/trim)
(clojure.string/split $ #"\n")
(mapv read-string $)
(day5a $))
;;343364
)


(defn day5b
  {:test (fn []
           (clojure.test/is (= (day5b [0 3 0 1 -3]) 10)))
  }
  [offsets]
  (loop [offsets offsets idx 0 steps 0]
    (if (< -1 idx (count offsets))
        (recur (update offsets idx (if (< (nth offsets idx) 3) inc dec))
               (+ idx (nth offsets idx))
               (inc steps))
        steps))
)

(comment (as-> (slurp "data/input_day5.txt") $
;;(clojure.string/trim)
(clojure.string/split $ #"\n")
(mapv read-string $)
(day5b $))
;;25071947
)
