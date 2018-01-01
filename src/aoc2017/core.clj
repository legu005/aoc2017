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


(defn day3a
  {:test (fn []
    (clojure.test/is (= (day3a 1) 0))
    (clojure.test/is (= (day3a 12) 3))
    (clojure.test/is (= (day3a 23) 2))
    (clojure.test/is (= (day3a 1024) 31)))}
  [square]
  (loop [[x y] [0 0]
         [dx dy] [1 0]
         pos 1]
     (let [absx (max x (- x)) absy (max y (- y))]
       (if (= pos square)
         (+ absx absy)
         (let [[dx dy] (if (or (and (= absx absy) (not (<= y 0 x))) (and (= absx (inc absy)) (<= y 0 x)))
                         [(- dy) dx]
                         [dx dy])]
           (recur [(+ x dx) (+ y dy)] [dx dy] (inc pos)))))))

(comment (day3a 312051)
;;430
)

(defn day3b
  {:test (fn []
    (clojure.test/is (= (day3b 0) 1))
    (clojure.test/is (= (day3b 20) 23))
    (clojure.test/is (= (day3b 25) 26))
    (clojure.test/is (= (day3b 800) 806)))}
  [test-value]
  (loop [[x y] [1 0]
         [dx dy] [1 0]
         grid {[0 0] 1}]
     (let [absx (max x (- x))
           absy (max y (- y))
           square-value (reduce (fn [sum dir]
                                  (+ sum (get grid (mapv + [x y] dir) 0)))
                                0
                                [[1 0] [1 1] [0 1] [-1 1]
                                 [-1 0] [-1 -1] [0 -1] [1 -1]])]
       (if (> square-value test-value)
         square-value
         (let [[dx dy] (if (or (and (= absx absy) (not (<= y 0 x)))
                               (and (= absx (inc absy)) (<= y 0 x)))
                         [(- dy) dx]
                         [dx dy])]
           (recur [(+ x dx) (+ y dy)] [dx dy] (assoc grid [x y] square-value)))))))

(comment (day3b 312051)
;;312453
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


(defn day6a-redist
  {:test (fn []
           (clojure.test/is (= (day6a-redist [0 2 7 0]) [2 4 1 2])))
  }
  [banks]
  (let [idx
        ;; (apply max-key (fn [i] (nth banks i)) (range (count banks)))
        (.indexOf banks (apply max banks))
        ]
    (loop [banks2 (assoc banks idx 0)
           idx idx
           amount (nth banks idx)]
           (if (= amount 0)
           banks2
           (let [idx (mod (inc idx) (count banks2))]
             (recur (update banks2 idx inc) idx (dec amount)))))))


(defn day6a
  {:test (fn []
           (clojure.test/is (= (day6a [0 2 7 0]) 5)))
  }
  [banks]
  (println  banks)
  (loop [banks banks states #{} steps 0]
    (if (contains? states banks)
      steps
      (recur (day6a-redist banks) (conj states banks) (inc steps)))
  )
)

(comment (as-> (slurp "data/input_day6.txt") $
;;(clojure.string/trim)
(clojure.string/split $ #"\t")
(mapv read-string $)
(day6a $))
;;11137
)


(defn day6b
  {:test (fn []
           (clojure.test/is (= (day6b [0 2 7 0]) 4)))
  }
  [banks]
  (println  banks)
  (loop [banks banks states {} steps 0]
    (if (contains? states banks)
      (- steps (get states banks))
      (recur (day6a-redist banks) (conj states {banks steps}) (inc steps)))
  )
)

(comment (as-> (slurp "data/input_day6.txt") $
;;(clojure.string/trim)
(clojure.string/split $ #"\t")
(mapv read-string $)
(day6b $))
;;11137
)


(defn day7a
  {:test (fn []
           (clojure.test/is (= (day7a ["pbga (66)"
                                        "xhth (57)"
                                        "ebii (61)"
                                        "havc (66)"
                                        "ktlj (57)"
                                        "fwft (72) -> ktlj, cntj, xhth"
                                        "qoyq (66)"
                                        "padx (45) -> pbga, havc, qoyq"
                                        "tknk (41) -> ugml, padx, fwft"
                                        "jptl (61)"
                                        "ugml (68) -> gyxo, ebii, jptl"
                                        "gyxo (61)"
                                        "cntj (57)"]) "tknk")))
  }
  [progs]
  (let [[parents children]
        (reduce (fn [[parents children] prog]
            (if (clojure.string/includes? prog "->")
              [(->> (clojure.string/split prog #" " 2)
                    (first)
                    (conj parents))
               (as-> (clojure.string/split prog #"->") $
                     (last $)
                     (clojure.string/trim $)
                     (clojure.string/split $ #",")
                     (map clojure.string/trim $)
                     (apply conj children $))]
              [parents children]))
          [#{} #{}]
          progs)]
        (-> (clojure.set/difference parents children)
            (first)))
  )


  (comment (as-> (slurp "data/input_day7.txt") $
  ;;(clojure.string/trim)
  (clojure.string/split $ #"\n")
  (day7a $))
  ;;"fbgguv"
  )



(defn day7b-get-weight
  [m h]
  (let [p (get m h) d (get p :disc) w (get p :weight)]
    ;;(println p d w)
    (if d
      (let [ws (mapv #(day7b-get-weight m %) d)
            cw (reduce (fn [a [w tw cw]] (+ a cw)) 0 ws)]
        ;;(println ws cw)
        (cond
          (not= cw 0)
            [0 0 cw]
          (apply = (map (fn [[w tw cw]] tw) ws))
            [w (reduce (fn [a [w tw cw]] (+ a tw)) w ws) 0]
          :else
            (as-> (frequencies (map (fn [[w tw cw]] tw) ws)) $
                  (let [ctw (key (apply max-key val $))
                        itw (key (apply min-key val $))
                        iw (ffirst (filter (fn [[w tw cw]] (= tw itw)) ws))]
                    [0 0 (+ iw (- ctw itw))]))))
      [w w 0])))

(defn day7b
  {:test (fn []
           (clojure.test/is (= (day7b ["pbga (66)"
                                        "xhth (57)"
                                        "ebii (61)"
                                        "havc (66)"
                                        "ktlj (57)"
                                        "fwft (72) -> ktlj, cntj, xhth"
                                        "qoyq (66)"
                                        "padx (45) -> pbga, havc, qoyq"
                                        "tknk (41) -> ugml, padx, fwft"
                                        "jptl (61)"
                                        "ugml (68) -> gyxo, ebii, jptl"
                                        "gyxo (61)"
                                        "cntj (57)"]) 60)))
  }
  [progs]
  (let [m (reduce
    (fn [m s]
      (as-> (clojure.string/replace s #"[\(\),\->]" "") $
            (clojure.string/replace $ #"  " " ")
            (clojure.string/split $ #" ")
            (assoc {} :name (first $) :weight (read-string (second $)) :disc (next (next $)))
            (assoc m (:name $) $)))
    {}
    progs)
    p (set (keys m))
    c (set (filter some? (flatten (map :disc (vals m)))))
    h (first (clojure.set/difference p c))]
    (last (day7b-get-weight m h))
    ;;(println m)
    ;;(println h)
  )
)


(comment (as-> (slurp "data/input_day7.txt") $
;;(clojure.string/trim)
;;(clojure.string/split $ #"\n")
(clojure.string/split-lines $)
(day7b $))
;;"fbgguv"
)
