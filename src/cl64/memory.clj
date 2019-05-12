(ns cl64.memory
  (:require [cl64.computer :refer :all]))



(defn mget 
  ([c]       (get (:mem c) (:address c)))
  ([c data] 
    (let  [opcode (get data 0) lo (get data 1)]
      (cond 
        (contains? implied-mode-ops opcode) 	 nil 
        (contains? immediate-mode-ops opcode) lo
        (contains? relative-mode-ops opcode) lo
        :else (mget c))))) 


(defn mget-bytes [c len] (subvec (:mem c) (:address c) (+ (:address c) len)))
(defn mget-word [c]  (wordify (mget c) (mget (assoc c :address (+ (:address c) 1)))))
(defn mpeek [c address] (get (:mem c) address))
(defn mpeek-bytes [c address len] (mget-bytes (assoc c :address address) len))
(defn mput [c val]   (assoc (assoc-in c [:mem (:address c)] (byteify val)) :value (byteify val)))

(defn mload
  "replaces a sequence of bytes starting at address in memory"
  [c address bytes]
  (let [rest-address (+ address (count bytes)) memory-length (count (:mem c))]
    (if (<= (+ address (count bytes)) memory-length)
      (assoc c :mem (into [] (concat (subvec (:mem c) 0 address) bytes 
        (if (< rest-address memory-length) (subvec (:mem c) rest-address memory-length) []))))
      c)))



(defn get-byte-string 
  [bytes] 
  (reduce (fn [rs v] (str rs (format "%02X " v))) "" bytes))

(defn get-byte-output-string
  [bytes]
  (reduce (fn [rs v] (str rs (if (and (> v 32) (< v 128)) (char v) "."))) "" bytes))


(defn dump-page
  "returns a string based on the bytes of memory at the given address"
  [c address]
  (reduce (fn [rs a]
    (let [ra (+ address (* a 16)) bytes (mpeek-bytes c ra 16)]
      (str rs (format "%04X: %s| %s\n" ra (get-byte-string bytes) (get-byte-output-string bytes))))) "" (range 16)))
