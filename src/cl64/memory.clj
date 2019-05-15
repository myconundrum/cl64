(ns cl64.memory
  (:require [cl64.computer :refer :all]))


(defn -peek [c mmap address] (get (:mem c) address))
(defn -poke [c mmap address val] (assoc-in c [:mem address] (byteify val)))


(defn add-memory-map [c map] (assoc c :mem-maps (conj (:mem-maps c) map)))

(def base-memory-map {:from 0 :to 0xffff :active true :peekfn -peek :pokefn -poke :data nil})

(defn get-memory-map
  [c address]
  (reduce (fn [rm m] (if (and (:active m) (>= address (:from m)) (< address (:to m))) m rm)) base-memory-map  
    (:mem-maps c)))

(defn set-memory-map-active
  [c name state]
  (assoc c :mem-maps
    (into [] (map (fn [m] (if (= (:name m) name) (assoc m :active state) m)) (:mem-maps c)))))

(defn mem-unmapped-peek 
  ([c address] (-peek c base-memory-map address))
  ([c mmap address] (-peek c base-memory-map address)))

(defn mem-unmapped-poke 
  ([c address val] (-poke c base-memory-map address val))
  ([c mmap address val] (-peek c base-memory-map address val)))


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
