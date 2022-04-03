(ns wordle.solver
  (:require [clojure.string :as str]))

(def wordlist-path "resources/data/wordle-answers-alphabetical.txt")
(def wordlist-guesses-path "resources/data/wordle-allowed-guesses.txt")

(defn words [f]
  (->> (slurp f)
       (clojure.string/split-lines)))

(defonce answers (set (words wordlist-path )))
(defonce guesses (set (words wordlist-guesses-path )))

(defn tf-char-at
  "returns a transducer performing `f` when the character `c` is at
  position `idx` "
  [f idx c]
  (f (fn [w]
       (= c (.charAt w idx)))))

(defn zip-range [w]
  (map #(list %1 %2) (range) w))

(defn tf-clue [f clue]
  (->> (str/lower-case clue)
      (zip-range)
      (remove #(= \- (second %)))
      (map #(tf-char-at f (first %) (second %)))
      (apply comp)))


(defn characters [clues]
  (let [chars (->> (map str/lower-case clues)
                   (map set)
                   (apply clojure.set/union))]
    (disj chars \-)))


(defn filter-words [words {:keys [correct present wrong]}]
  (sequence (comp (tf-clue filter correct)
                  (->> (map #(tf-clue remove %) present)
                       (apply comp))
                  (filter #(every? (set %) (characters present)))
                  (remove #(some   (set %) (characters wrong))))
            words))

(defn clue [word guess]
  (let [w (set word)]
    {:correct (str/join (map (fn [a b] (if (= a b) a "-")) word guess))
     :present [(str/join (map (fn [a b] (if (and (not= a b) (w b)) b "-")) word guess))]
     :wrong   (str/join (remove w guess))}))

(defn best-guess [words guesses n]
  (if (zero? n)
    [(count words) nil]
    (case (count words)
      2 [2 (first words)]
      1 [1 (first words)]
      (->> words
           (pmap (fn [g]
                  [(transduce (map  (fn [w]
                                       (if (= w g)
                                         1
                                         (let [words*   (-> (filter-words words (clue w g))
                                                            set
                                                            (disj g))]
                                           (first (best-guess words* words* (dec n) ))))))
                               +
                               words)
                   g]))
           (apply min-key first)))))

(defn example-1 []
  ;; still a bug here, the clues indicate there should be at least 2 'e's in the word
  (let [clues {:correct "-e"
               :present ["-r" "----e"]
               :wrong "pylonc"}
        words (filter-words answers clues)]
    (prn (count words) words)
    (best-guess words words 6)))
