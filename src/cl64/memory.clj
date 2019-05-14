(ns cl64.memory
  (:require [cl64.computer :refer :all]))



(defn mem-peek
  [c address]
  (get (:mem c) address))

(defn mem-peek-word
  [c address]
  (wordify (mem-peek c address) (mem-peek c (inc address))))

(defn mem-peek-bytes
  [c address len]
  (into [] (map  (fn [v] (mem-peek c (+ address v))) (range len))))

(defn mem-poke
  [c address val]
  (assoc-in c [:mem address] (byteify val)))

(defn get-byte-string 
  [bytes] 
  (reduce (fn [rs v] (str rs (format "%02X " v))) "" bytes))

(defn get-byte-output-string
  [bytes]
  (reduce (fn [rs v] (str rs (if (and (> v 32) (< v 128)) (char v) "."))) "" bytes))

