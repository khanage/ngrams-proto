(ns ngram.core
  (:use ngram.data)
  (:require [clojure.string :as s]))

(defmulti sanitise class)
(defmethod sanitise clojure.lang.Sequential [strs]
  (let [composed-f
        (comp
         #(s/replace % "- " "-")
         #(s/replace % #" {2,}" " ")
         #(s/replace % #"\.|," "")
         #(s/join " " %)
         #(map s/trim %)
         s/split-lines
         s/lower-case)]
    (apply str (s/join " " (map composed-f strs)))))
(defmethod sanitise String [str] (sanitise [str]))
(defmethod sanitise :default [arg]
  (throw (IllegalArgumentException.
          (str "I don't know what to do with [" (str arg) "]."))))

(defn to-words [str] (s/split str #" "))

(defn ngrams ([seq n] (ngrams seq 1 n))
  ([seq i n]
     (cond (< n 1) []
           (= n 1) seq
           :else (let [grams (range i (inc n))]
                   (mapcat #(partition % 1 seq) grams)))))

(defn string-ngrams [str & args]
  (let [words (to-words (sanitise str))]
    (apply ngrams (cons words args))))

(defn frequencies-and-ngrams [& args]
  (frequencies (apply ngrams args)))

(defn not-stop-word [word]
  (not (or (= word "the")
           (= word "a")
           (= word "or")
           (= word "this")
           (= word "in")
           (= word "no")
           (= word "to"))))

(defn filter-ngrams
  [grams]
  (let [f (fn [[gram c]]
            (let [first-gram (first gram)
                  last-gram (last gram)]
              (not (or
                    (= "the" last-gram)
                    (= "is" first-gram)
                    (= "is" last-gram)))))]
    (filter f grams)))

(comment
  (defn filt [[gram c]]
    (not (= "the" (last gram))))
  )

(defn filtered-ngrams [& args]
  (filter-ngrams (apply ngrams args)))

(defn top-n-ngrams [n strs]
  (let [occurs-and-len (fn [[grams occurs]] [occurs (count grams)])
        rev-sort-by (comp reverse sort-by)
        words (to-words (sanitise strs))
        filtered-words (filter #(not-stop-word %) words)]
    (take n (filter-ngrams (rev-sort-by occurs-and-len (frequencies-and-ngrams words 2 6))))))
