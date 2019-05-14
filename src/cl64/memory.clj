(ns cl64.memory
  (:require [cl64.computer :refer :all]))


(defn -peek [c mmap address] (get (:mem c) address))
(defn -poke [c mmap address val] (assoc-in c [:mem address] (byteify val)))
(def base-memory-map {:from 0 :to 0xffff :active true :peekfn -peek :pokefn -poke :data nil})

(defn get-memory-map
  [c address]
  (reduce (fn [rm k] (let [m (get (:mem-maps c) k)] (if (and (:active m) (>= address (:from m)) (< address (:to m))) m rm))) base-memory-map  
    (keys (:mem-maps c))))
  
(defn mem-peek
  "peek into memory, with support for memory maps"
  [c address]
  (let [mmap (get-memory-map c address)]
    ((:peekfn mmap) c mmap address)))
 

(defn mem-peek-word
  [c address]
  (wordify (mem-peek c address) (mem-peek c (inc address))))

(defn mem-peek-bytes
  [c address len]
  (into [] (map  (fn [v] (mem-peek c (+ address v))) (range len))))

(defn mem-poke
  [c address val]
  (let [mmap (get-memory-map c address)]
    ((:pokefn mmap) c mmap address val)))
 

(defn get-byte-string 
  [bytes] 
  (reduce (fn [rs v] (str rs (format "%02X " v))) "" bytes))

(defn get-byte-output-string
  [bytes]
  (reduce (fn [rs v] (str rs (if (and (> v 32) (< v 128)) (char v) "."))) "" bytes))
