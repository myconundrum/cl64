(ns cl64.core
  (:gen-class))

(require '[clojure.test :refer :all])

(def stack-base 0x0100)
(def nmi-address 0xfffa)
(def reset-address 0xfffc)
(def break-address 0xfffe)
(def memory-size 0x10000) 

(def immediate-mode-ops  #{0x69 0x29 0xc9 0xe0 0xc0 0x49 0xa9 0xa2 0xa0 0x09 0xe9})
(def zeropage-mode-ops  #{0x65 0x25 0x06 0x24 0xc5 0xe4 0xc4 0xc6 0x45 0xe6 0xa5 0xa6 0xa4 0x46 0x05 0x26 0x66 0xe5 0x85 0x86 0x84})
(def zeropagex-mode-ops #{0x75 0x35 0x16 0xd5 0xd6 0x55 0xf6 0xb5 0xb4 0x56 0x15 0x36 0x76 0xf5 0x95 0x94})
(def zeropagey-mode-ops  #{0xb6 0x96})
(def absolute-mode-ops  #{0x6d 0x2d 0x0e 0x2c 0xcd 0xec 0xcc 0xce 0x4d 0xee 0x4c 0x20 0xad 0xae 0xac 0x4e 0x0d 0x2e 0x6e 0xed 0x8d 0x8e 0x8c})
(def absolutex-mode-ops #{0x7d 0x3d 0x1e 0xdd 0xde 0x5d 0xfe 0xbd 0xbc 0x5e 0x1d 0x3e 0x7e 0xfd 0x9d})
(def absolutey-mode-ops  #{0x79 0x39  0xd9 0x59 0xb9 0xbe 0x19 0xf9 0x99})
(def indirect-mode-ops  #{0x6c})
(def indexed-indirect-mode-ops #{0x61 0x21 0xc1 0x41 0xa1 0x01 0xe1 0x81})
(def indirect-indexed-mode-ops #{0x71 0x31 0xd1 0x51 0xb1 0x11 0xf1 0x91})
(def implied-mode-ops #{0x4a 0x0a 0x2a 0x6a 0x00 0x18 0xd8 0x58 0xb8 0xca 0x88 0xe8 0xc8 0xea 0x48 0x08 0x68 0x28 0x40 0x60 0x38 0xf8 0x78 0xaa 0xa8 0xba 0x8a 0x9a 0x98})
(def relative-mode-ops #{0x90 0xb0 0xf0 0x30 0xd0 0x10 0x50 0x70})

(defn make-comp [] 
    {:a 0 :x 0 :y 0 :sp 0xff :p 0x24 :pc 0     ; registers
    	:mem (vec (replicate memory-size 0))      ; memory 
     :address nil                              ; computed address lines
     })

(defn byteify [value] (bit-and value 0xff))
(defn wordify 
  "takes two bytes in little endian format and turns them into a big endian word."
  [lo hi]
  (bit-or lo (bit-shift-left hi 8)))

(defn mget 
  ([c]       (get (:mem c) (:address c)))
  ([c data] 
    (let  [opcode (get data 0) lo (get data 1)]
      (cond 
        (contains? implied-mode-ops opcode) 	 nil 
        (contains? immediate-mode-ops opcode) lo
        (contains? relative-mode-ops opcode) lo
        :else (mget c))))) 

(defn mpeek [c address] (get (:mem c) address))
(defn mget-bytes [c len] (subvec (:mem c) (:address c) (+ (:address c) len)))
(defn mget-word [c]  (wordify (mget c) (mget (assoc c :address (+ (:address c) 1)))))
(defn mput [c val]   (assoc (assoc-in c [:mem (:address c)] (byteify val)) :value (byteify val)))
(defn rget [c register] (get c register))
(defn rput [c register val] (assoc c register val :value val))
(defn radd [c register val] (assoc c register (+ (rget c register) val)))

(defn push [c val] (radd (assoc (assoc-in c [:mem (+ stack-base (rget c :sp))] (byteify val)) :value (byteify val)) :sp -1))
(defn pull [c reg] 
  (let [c (radd c :sp 1)]
    (rput c reg (get (:mem c) (+ stack-base (rget c :sp))))))
(defn push-word [c val] (push (push c (byteify (bit-shift-right val 8))) (byteify (val))))
(defn pull-word [c reg] 
  (let [clo (pop c reg) chi (pop clo reg)] 
    (rput c reg (bit-or (rget clo reg) (bit-shift-left (rget chi reg) 8)))))

(def flags {:c 0x01 :z 0x02 :i 0x04 :d 0x08 :b 0x10 :u 0x20 :v 0x40 :n 0x80})
(defn flag [f] (get flags f))
(defn flag-set? [c f] (> (bit-and (:p c) (flags f)) 0))
(defn flag-clear? [c f] (not (flag-set? c f)))
(defn fset [c f]  (rput c :p (bit-or (flag f) (rget c :p))))
(defn fclear [c f] (rput c :p (bit-and (bit-not (flag f)) (rget c :p)))) 
(defn flag-if-zero [c f v] (if (= v 0) (fset c f) (fclear c f)))
(defn flag-if-nonzero [c f v] (if (not (= v 0)) (fset c f) (fclear c f)))
(defn flag-if-has-flag [c f v] (flag-if-nonzero c f (bit-and (flag f) v)))
(defn carry-if-bit [c bit v] (if (> (bit-and bit v) 0 ) (fset c :c) (fclear c :c)))
(defn set-bit-to-carry [c bit v] (if (flag-set? c :c) (bit-or v bit) v))
(defn set-flags
  "based on value and flags list, set the appropriate flags in the cpu"
  [c fs]
  (reduce (fn [rc f] (cond
    (= f :z) (flag-if-zero rc f (:value c))
    (= f :n) (flag-if-has-flag rc f (:value c))
    :else (flag-if-nonzero rc f (:value c)))) c fs))

(defn set-address [c address] (assoc c :address address))
 
(defn set-address-from-mode
  "determines an address anby determining the address mode of a given instruction"
  [c data]
  (let [opcode (get data 0) lo (get data 1) hi (get data 2)]
    (set-address c 
		    (cond 
		      (contains? implied-mode-ops opcode) 		nil
		      (contains? immediate-mode-ops opcode) nil
		      (contains? zeropage-mode-ops opcode) lo
		      (contains? relative-mode-ops opcode) lo
		      (contains? zeropagex-mode-ops opcode) (+ (rget c :x) lo)
		      (contains? zeropagey-mode-ops opcode) (+ (rget c :y) lo)
		      (contains? absolute-mode-ops opcode) (wordify lo hi)
		      (contains? absolutex-mode-ops opcode) (+ (wordify lo hi) (rget c :x))
		      (contains? absolutey-mode-ops opcode) (+ (wordify lo hi) (rget c :y))
		      (contains? indirect-mode-ops opcode) (mget-word c (wordify lo hi))
		      (contains? indexed-indirect-mode-ops opcode) (mget-word c (+ lo (rget c :x))) 
		      (contains? indirect-indexed-mode-ops opcode) (+ (mget-word c lo) (rget c :y))))))

(defn bittest
  [c d]
  (assoc (flag-if-has-flag (flag-if-has-flag c :n (mget c d)) 
    :v (mget c d)) :value (bit-and (mget c d) (rget c :a))))

(defn get-relative [v] (if (> (bit-and v :n) 0) (* (+ (bit-not v) 1) -1) v))
     
(def opcodes
  [{:name "LDA" :ops #{0xa9 0xa5 0xb5 0xad 0xbd 0xb9 0xa1 0xb1} :flags [:z :n] :fn (fn [c d] (rput c :a (mget c d)))}
   {:name "LDY" :ops #{0xa0 0xa4 0xb4 0xac 0xbc} :flags [:z :n] :fn (fn [c d] (rput c :y (mget c d)))}
   {:name "LDX" :ops #{0xa2 0xa6 0xb6 0xae 0xbe} :flags [:z :n] :fn (fn [c d] (rput c :x (mget c d)))}
   {:name "STA" :ops #{0x85 0x95 0x8d 0x9d 0x99 0x81 0x91} :fn (fn [c d] (mput c (rget c :a)))}
   {:name "STX" :ops #{0x86 0x96 0x8e} :fn (fn [c d] (mput c (rget c :x)))}
   {:name "STY" :ops #{0x84 0x94 0x8c} :fn (fn [c d] (mput c (rget c :y)))}
   {:name "TAX" :ops #{0xaa} :flags [:z :n] :fn (fn [c d] (rput c :x (rget c :a)))}
   {:name "TAY" :ops #{0xa8} :flags [:z :n] :fn (fn [c d] (rput c :y (rget c :a)))}
   {:name "TSX" :ops #{0xba} :flags [:z :n] :fn (fn [c d] (rput c :x (rget c :sp)))}
   {:name "TXA" :ops #{0x8a} :flags [:z :n] :fn (fn [c d] (rput c :a (rget c :x)))}
   {:name "TYA" :ops #{0x98} :flags [:z :n] :fn (fn [c d] (rput c :a (rget c :y)))}
   {:name "TXS" :ops #{0xaa} :flags [:z :n] :fn (fn [c d] (rput c :sp (rget c :x)))}
   {:name "JMP" :ops #{0x4c 0x6c} :fn (fn [c d] (rput c :pc (:address c)))}
   {:name "PHP" :ops #{0x08} :fn (fn [c d] (push c :p))}
   {:name "PHA" :ops #{0x48} :fn (fn [c d] (push c :a))}
   {:name "PLP" :ops #{0x28} :fn (fn [c d] (pull c :p))}
   {:name "PLA" :ops #{0x68} :flags [:z :n] :fn (fn [c d] (pull c :a))}
   {:name "CLV" :ops #{0xb8} :fn (fn [c d] (fclear c :v))}
   {:name "CLI" :ops #{0x58} :fn (fn [c d] (fclear c :i))}
   {:name "CLD" :ops #{0xd8} :fn (fn [c d] (fclear c :d))}
   {:name "CLC" :ops #{0x18} :fn (fn [c d] (fclear c :c))}
   {:name "SEI" :ops #{0x78} :fn (fn [c d] (fset c :i))}
   {:name "SED" :ops #{0xf8} :fn (fn [c d] (fset c :d))}
   {:name "SEC" :ops #{0x38} :fn (fn [c d] (fset c :c))}
   {:name "INX" :ops #{0xe8} :flags [:z :n] :fn (fn [c d] (radd c :x 1))}
   {:name "INY" :ops #{0xc8} :flags [:z :n] :fn (fn [c d] (radd c :y 1))}
   {:name "DEX" :ops #{0xca} :flags [:z :n] :fn (fn [c d] (radd c :x -1))}
   {:name "DEY" :ops #{0x88} :flags [:z :n] :fn (fn [c d] (radd c :y -1))}
   {:name "INC" :ops #{0xe6 0xf6 0xee 0xfe} :flags [:z :n] :fn (fn [c d] (mput c (+ (mget c d) 1)))}
   {:name "DEC" :ops #{0xc6 0xd6 0xce 0xde} :flags [:z :n] :fn (fn [c d] (mput c (+ (mget c d) -1)))}
   {:name "AND" :ops #{0x29 0x25 0x35 0x2d 0x3d 0x39 0x21 0x31} :flags [:z :n] :fn (fn [c d] (rput c :a (bit-and (rget c :a) (mget c d))))}
   {:name "ORA" :ops #{0x09 0x05 0x15 0x0d 0x1d 0x19 0x01 0x11} :flags [:z :n] :fn (fn [c d] (rput c :a (bit-or (rget c :a) (mget c d))))}
   {:name "EOR" :ops #{0x49 0x45 0x55 0x4d 0x5d 0x59 0x41 0x51} :flags [:z :n] :fn (fn [c d] (rput c :a (bit-xor (rget c :a) (mget c d))))}
   {:name "BIT" :ops #{0x24 0x2c} :flags [:z] :fn bittest}
   {:name "BCC" :ops #{0x90} :fn (fn [c d] (if (flag-clear? :c) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BCS" :ops #{0xb0} :fn (fn [c d] (if (flag-set? :c) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BEQ" :ops #{0xf0} :fn (fn [c d] (if (flag-set? :z) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BNE" :ops #{0xd0} :fn (fn [c d] (if (flag-clear? :z) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BMI" :ops #{0x30} :fn (fn [c d] (if (flag-set? :n) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BPL" :ops #{0x10} :fn (fn [c d] (if (flag-clear? :n) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BVC" :ops #{0x90} :fn (fn [c d] (if (flag-clear? :v) (radd c :pc (get-relative (mget c d))) c))}
   {:name "BVS" :ops #{0xb0} :fn (fn [c d] (if (flag-set? :v) (radd c :pc (get-relative (mget c d))) c))}
   {:name "ASL" :ops #{0x0a} :flags [:z :n] :fn (fn [c d] (let [v (rget c :a)] (carry-if-bit (rput c :a (bit-shift-left v 1)) v 0x80)))}
   {:name "ASL" :ops #{0x06 0x16 0x0e 0x1e} :flags [:z :n] :fn (fn [c d] (let [v (mget c d)] (carry-if-bit (mput c (bit-shift-left v 1)) 0x80 v)))}
   {:name "LSR" :ops #{0x4a} :flags [:z :n] :fn (fn [c d] (let [v (rget c :a)] (carry-if-bit (rput c :a (bit-shift-right v 1)) v 0x01)))}
   {:name "LSR" :ops #{0x46 0x56 0x4e 0x5e} :flags [:z :n] :fn (fn [c d] (let [v (mget c d)] (carry-if-bit (mput c (bit-shift-right v 1)) 0x01 v)))}
   {:name "ROL" :ops #{0x2a} :flags [:z :n] :fn (fn [c d] (let [v (rget c :a)] (carry-if-bit (rput c :a (set-bit-to-carry (bit-shift-left v 1) 0x01)) v 0x80)))}
   {:name "ROL" :ops #{0x26 0x36 0x2e 0x3e} :flags [:z :n] :fn (fn [c d] (let [v (mget c d)] (carry-if-bit (mput c (set-bit-to-carry (bit-shift-left v 1) 0x01)) v 0x80)))}
   {:name "ROR" :ops #{0x6a} :flags [:z :n] :fn (fn [c d] (let [v (rget c :a)] (carry-if-bit (rput c :a (set-bit-to-carry (bit-shift-right v 1) 0x80)) v 0x01)))}
   {:name "ROR" :ops #{0x66 0x76 0x6e 0x7e} :flags [:z :n] :fn (fn [c d] (let [v (mget c d)] (carry-if-bit (mput c (set-bit-to-carry (bit-shift-right v 1) 0x80)) v 0x01)))}
   {:name "NOP" :ops #{0xea} :fn (fn [c d] c)}
   {:name "CMP" :ops #{0xc9 0xc5 0xd5 0xcd 0xdd 0xd9 0xc1 0xd1} :flags [:z :n] :fn (fn [c d] (let [comp (- (rget c :a) (mget c d))] (assoc (if (> comp 0) (fset c :c) c) :value comp)))}
   {:name "CPX" :ops #{0xe0 0xe4 0xec} :flags [:z :n] :fn (fn [c d] (let [comp (- (rget c :x) (mget c d))] (assoc (if (> comp 0) (fset c :c) c) :value comp)))}
   {:name "CPY" :ops #{0xe0 0xe4 0xec} :flags [:z :n] :fn (fn [c d] (let [comp (- (rget c :y) (mget c d))] (assoc (if (> comp 0) (fset c :c) c) :value comp)))}
   {:name "JSR" :ops #{0x20} :fn (fn [c d] (push-word (- (rget c :pc) 1)))} 
   {:name "RTS" :ops #{0x60} :fn (fn [c d] (let [c (pull-word c :pc)] (radd c :pc 1)))}])
     
(defn get-opcode [opcode]  (reduce (fn [rop op] (if (contains? (:ops op) opcode) op rop)) nil opcodes))

(defn mload
  "replaces a sequence of bytes starting at address in memory"
  [c address bytes]
  (let [rest-address (+ address (count bytes)) memory-length (count (:mem c))]
    (if (<= (+ address (count bytes)) memory-length)
      (assoc c :mem (into [] (concat (subvec (:mem c) 0 address) bytes 
        (if (< rest-address memory-length) (subvec (:mem c) rest-address memory-length) []))))
      c)))

(def three-byte-reads (conj absolute-mode-ops absolutex-mode-ops absolutey-mode-ops indirect-mode-ops))

(defn fetch 
  "gets opcode and any data at the pc"
  [c]
  (let [c (set-address c (rget c :pc))  opcode (mget c)]
    (cond 
      (contains? implied-mode-ops opcode) [opcode]
      (contains? three-byte-reads opcode) (mget-bytes c 3)
      :else (mget-bytes c 2))))

(defn show-instruction [data op] (println (:name op) data))

(defn show-computer
  "print out current cpu state"
  [c]
  (println (format "A: %X X: %X Y: %X PC: %X SP: %X P: %X" (:a c) (:x c) (:y c) (:pc c) (:sp c) (:p c)))
  (println "(N V - B D I Z C)")
  (println (take-last 8 (concat (repeat 8 \0) (Integer/toBinaryString (:p c)))))
  c)

(defn exec
  "fetches op and data at pc and emulates instruction"
  [c]
  (let [
    data (fetch c)                                  ; direct data from the program counter
    op (get-opcode (get data 0))                    ; op record given byte at PC
    c (set-address-from-mode (radd c :pc (count data)) data)] ; computer after fetch and address lines
    (show-computer (set-flags ((:fn op) c data) (:flags op)))))

(def halt 0xff)
(defn run-computer
  [c]
  (if (= halt (mpeek c (rget c :pc)))
    (println "hit halt.")
    (run-computer (exec c))))

(defn tcomp [] (run-computer (mload (make-comp) 0 [0xa2 0x30 0xa0 0x02 0x96 0x20 0xa5 0x22 0xFF])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println "helloworld"))

