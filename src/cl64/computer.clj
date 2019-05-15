(ns cl64.computer
  (:require [clojure.java.io :as io]))

;
; 6502/C64 constant values
;
(def stack-base 0x0100)
(def nmi-address 0xfffa)
(def reset-address 0xfffc)
(def break-address 0xfffe)
(def memory-size 0x10000) 



(defn byteify [value] (bit-and value 0xff))
(defn wordify 
  "takes two bytes in little endian format and turns them into a big endian word."
  [lo hi]
  (bit-or lo (bit-shift-left hi 8)))
(defn twos-complement [value] (* (+ (byteify (bit-not value)) 1) -1))

(defn make-computer [] 
    {:a 0 :x 0 :y 0 :sp 0xff :p 0x24 :pc 0     ; registers
    	:mem (vec (replicate memory-size 0))      ; memory 
    	:mem-maps []																														; memory maps
     :address nil                              ; computed address lines
     :value nil 																															; last read value
     :cycles 0																																	; cycle count
     })



(defn file-exists? [path] (and (not (.isDirectory (io/file path))) (.exists (io/file path))))


;
; from stack overflow.
; https://stackoverflow.com/questions/23018870/how-to-read-a-whole-binary-file-nippy-into-byte-array-in-clojure
;

(defn file-to-byte-array
  [^java.io.File file]
  (let [result (byte-array (.length file))]
    (with-open [in (java.io.DataInputStream. (clojure.java.io/input-stream file))]
      (.readFully in result))
    result))

(defn slurp-bytes
  [path]
  (if (file-exists? path) 
    (into [] (map (fn [v] (byteify v)) (file-to-byte-array (io/file path))))
    []))





