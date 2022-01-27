(ns wardle.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))


;;;;
;; Dictionaries
;;;;

(defn characters []
  (->> (range 26)
       (map (comp char #(+ % (int \a))))))

(def wordlists {:corncob        "corncob_lowercase.txt"
                :output         "words_five.txt"
                :alpha-only     "words_alpha.txt"
                :wordle         "wordle-answers-alphabetical.txt"
                :wordle-guesses "wordle-allowed-guesses.txt"})

(defn wl [k]   (str "resources/data/" (wordlists k)))
(defn wordlist []  (slurp (wl :wordle)))
(defn guesses  []  (slurp (wl :wordle-guesses)))

;;;;
;; Wordlist Processing
;;;;

(defn process-wordlist! []
  (let [file-out (wl :output)]
    (io/delete-file file-out true)
    (with-open [rdr (io/reader (wl :corncob))]
      (doseq [w (->> (line-seq rdr)
                     (filter #(= 5 (count %))))]
        (spit file-out (str w "\n") :append true)))))

(defn words-without-repeats []
  (->> wordlist
       (clojure.string/split-lines)
       (set)
       (map frequencies)
       (filter #(= 5 (count %)))
       count))

(defn vectorize [wl]
  (->> (clojure.string/split-lines wl)
       (mapv vec)))

;;;;
;; Core
;;;;;

(defn in-word? [word c]
  (boolean (some #(= c %) word)))

(defn guess [answer guess]
  (map (fn [a b idx]
         (cond (= a b)             (list :g b idx)
               (in-word? answer b) (list :y b idx)
               :else               (list :r b)))
       answer guess (range 5)))

(defn prune-results
  ([facts]
   (prune-results (vectorize (wordlist)) facts))

  ([answers facts]
   (->> answers
        (filter (fn [w]
                  (every? #(case (first %)
                             :g (= (second %)
                                   (w (second (rest %))))
                             :y (and (in-word? w (second %))
                                     (not (= (second %)
                                             (w (second (rest %))))))
                             :r (not (in-word? w (second %))))
                          facts))))))

(comment (prune-results (guess "joker" "mandy")))

(defn cheat [{:keys [g y r]}]
  (->> (prune-results   (concat (map #(vector :g (first %) (second %)) g)
                                (map #(vector :y (first %) (second %)) y)
                                (map #(vector :r %) r)))
       (map (partial apply str))))

(comment (cheat (prune-results (guess "joker" "mandy"))))
(comment (cheat {:g [[\c 0] [\i 2]] :y [[\i 1] [\r 2]]:r ["salontedhk"]}))


(defn freqs []
  (let [fs (-> (wordlist)
               (frequencies)
               (dissoc \newline))]
    (->> (sort-by second fs)
         reverse
         (map first))))


;; ~ 182 results
;; (["eland" "riots"] ["enrol" "staid"] ["salon" "tried"] ["salon" "tired"] ["saint" "older"])
(comment (["alert" "sonic"] ["alert" "scion"] ["alter" "sonic"] ["alter" "scion"] ["ascot" "liner"] ["antic" "loser"] ["acorn" "islet"] ["resin" "octal"] ["renal" "stoic"] ["risen" "octal"] ["rinse" "octal"] ["octal" "resin"] ["octal" "risen"] ["octal" "rinse"] ["octal" "siren"] ["trail" "scone"] ["train" "close"] ["trial" "scone"] ["trice" "salon"] ["learn" "stoic"]))
(defn starting-guesses []
  (let [ws    (wordlist)
        words (set (clojure.string/split-lines ws))]
    (->> (freqs)
         (take 10)
         (combo/permutations)
         (keep (fn [ls]
                 (let [w1 (apply str (take 5 ls))
                       w2 (apply str (drop 5 ls))]
                   (when (and (words w1) (words w2))
                     [w1 w2]))))
         (take 20))))

;; Run time ~800s or 13 minutes
(defn simple-first-guess []
  (let [answers (vectorize (wordlist))
        guesses (vectorize (guesses))]
    (->> answers
         (filter #(some #{\a} %))
         (filter #(= 5 (count (frequencies %))))
         (pmap (fn [g]
                 (list (apply str g)
                       (transduce (map #(->> (guess % g)
                                             (prune-results answers)
                                              count))
                                  +
                                  answers))))
         (sort-by second)
         (take 20))))


(comment (("raise" 141217) ("arise" 147525) ("irate" 147649) ("arose" 152839) ("alter" 162031) ("saner" 162341) ("later" 162567) ("snare" 164591) ("stare" 165047) ("slate" 165691) ("alert" 165751) ("crate" 168763) ("trace" 171357) ("stale" 175023) ("aisle" 176377) ("learn" 177423) ("alone" 178633) ("leant" 178857) ("least" 181009) ("crane" 182287)))


;; Like `simple-first-guess` but slower :(.. see you in rust
(defn speedup-attempt []
  (let [answers   (vectorize (wordlist))
        guesses   (vectorize (guesses))
        pos-lkup  (->> (for [c (characters)]
                         {c (->> (for [n (range 5)]
                                   (let [res (mapv #(= (% n) c) answers)]
                                     {[n true]  (into-array res)
                                      [n false] (into-array (map not res))}))
                                 (into {}))})
                       (into {}))
        char-lkup (->> (for [c (characters)]
                         (let [res (mapv #(in-word? % c) answers)]
                           {[c true]  (into-array res)
                            [c false] (into-array (map not res))}))
                       (into {}))]
    (->> answers
         (take 1)
         (map (fn [g]
                [(apply str g)
                 (->> answers
                      (map (fn [w]
                             (->> (guess w g)
                                  (mapcat #(case (first %)
                                             :g (list (get-in pos-lkup [(nth % 1) [(nth % 2) true]]))
                                             :y (list
                                                 (get char-lkup [(nth % 1) true])
                                                 (get-in pos-lkup [(nth % 1) [(nth % 2) false]]))
                                             :r (list (get char-lkup [(nth % 1) false]))))
                                  (apply map (fn [& args]
                                               (every? true? args)))
                                  (remove false?)
                                  (count))))
                      (reduce +))]))
         (sort-by second))))
