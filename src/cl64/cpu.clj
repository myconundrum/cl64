(ns cl64.cpu
  (:require [cl64.computer :refer :all]
  										[cl64.memory :refer :all]))

(require '[clojure.set :refer :all])

;
; opcodes by addressing mode.
;
(def immediate-mode #{0x69 0x29 0xc9 0xe0 0xc0 0x49 0xa9 0xa2 0xa0 0x09 0xe9})
(def zeropage-mode #{0x65 0x25 0x06 0x24 0xc5 0xe4 0xc4 0xc6 0x45 0xe6 0xa5 0xa6 0xa4 0x46 0x05 0x26 0x66 0xe5 0x85 0x86 0x84})
(def zeropagex-mode #{0x75 0x35 0x16 0xd5 0xd6 0x55 0xf6 0xb5 0xb4 0x56 0x15 0x36 0x76 0xf5 0x95 0x94})
(def zeropagey-mode #{0xb6 0x96})
(def absolute-mode #{0x6d 0x2d 0x0e 0x2c 0xcd 0xec 0xcc 0xce 0x4d 0xee 0x4c 0x20 0xad 0xae 0xac 0x4e 0x0d 0x2e 0x6e 0xed 0x8d 0x8e 0x8c})
(def absolutex-mode #{0x7d 0x3d 0x1e 0xdd 0xde 0x5d 0xfe 0xbd 0xbc 0x5e 0x1d 0x3e 0x7e 0xfd 0x9d})
(def absolutey-mode #{0x79 0x39  0xd9 0x59 0xb9 0xbe 0x19 0xf9 0x99})
(def indirect-mode #{0x6c})
(def indexed-indirect-mode #{0x61 0x21 0xc1 0x41 0xa1 0x01 0xe1 0x81})
(def indirect-indexed-mode #{0x71 0x31 0xd1 0x51 0xb1 0x11 0xf1 0x91})
(def implied-mode #{0x4a 0x0a 0x2a 0x6a 0x00 0x18 0xd8 0x58 0xb8 0xca 0x88 0xe8 0xc8 0xea 0x48 0x08 0x68 0x28 0x40 0x60 0x38 0xf8 0x78 0xaa 0xa8 0xba 0x8a 0x9a 0x98})
(def relative-mode #{0x90 0xb0 0xf0 0x30 0xd0 0x10 0x50 0x70})

;
; Base cycle count by opcode
;
(def cycle-counts
  {0x69 2 0x65 3 0x75 4 0x6d 4 0x7d 4 0x79 4 0x61 6 0x71 5 0x29 2 0x25 3 0x35 4 0x2d 4
   0x3d 4 0x39 4 0x21 6 0x31 5 0x0a 2 0x06 5 0x16 6 0x0e 6 0x1e 7 0x90 2 0xb0 2 0xf0 2
   0x24 3 0x2c 4 0x30 2 0xd0 2 0x10 2 0x00 7 0x50 2 0x70 2 0x18 2 0xd8 2 0x58 2 0xb8 2 
   0xc9 2 0xc5 3 0xd5 4 0xcd 4 0xdd 4 0xd9 4 0xc1 6 0xd1 5 0xe0 2 0xe4 3 0xec 4 0xc0 2
   0xc4 3 0xcc 4 0xc6 5 0xd6 6 0xce 6 0xde 7 0xca 2 0x88 2 0x49 2 0x45 3 0x55 4 0x4d 4
   0x5d 4 0x59 4 0x41 6 0x51 5 0xe6 5 0xf6 6 0xee 6 0xfe 7 0xe8 2 0xc8 2 0x4c 3 0x6c 5
   0x20 6 0xa9 2 0xa5 3 0xb5 4 0xad 4 0xbd 4 0xb9 4 0xa1 6 0xb1 5 0xa2 2 0xa6 3 0xb6 4
   0xae 4 0xbe 4 0xa0 2 0xa4 3 0xb4 4 0xac 4 0xbc 4 0x4a 2 0x46 5 0x56 6 0x4e 6 0x5e 7
   0xea 2 0x09 2 0x05 3 0x15 4 0x0d 4 0x1d 4 0x19 4 0x01 6 0x11 5 0x48 3 0x08 3 0x68 4
   0x28 4 0x2a 2 0x26 5 0x36 6 0x2e 6 0x3e 7 0x6a 2 0x66 5 0x76 6 0x6e 6 0x7e 7 0x40 6
   0x60 6 0xe9 2 0xe5 3 0xf5 4 0xed 4 0xfd 4 0xf9 4 0xe1 6 0xf1 5 0x38 2 0xf8 2 0x78 2
   0x85 3 0x95 4 0x8d 4 0x9d 5 0x99 5 0x81 6 0x91 6 0x86 3 0x96 4 0x8e 4 0x84 3 0x94 4
   0x8c 4 0xaa 2 0xa8 2 0xba 2 0x8a 2 0x9a 2 0x98 2})

;
; These opcodes require an extra cycle on crossing a page boundary
;
(def page-boundary-opcodes
  #{0x7d 0x79 0x71 0x3d 0x39 0x31 0xdd 0xd9 0xd1 0x5d 0x59 0x51 0xbd 0xb9 0xb1 0xbe 0xbc 0x1d 0x19 0x11 0xfd 0xf9 0xf1 })


(defn add-cycles-by-op [c d] (assoc c :cycles (+ (:cycles c) (get cycle-counts (first d) 0))))
(defn inc-cycles [c] (assoc c :cycles (inc (:cycles c))))


(defn mget 
  ([c] (mem-peek c (:address c)))
  ([c data] 
    (let  [opcode (get data 0) lo (get data 1)]
      (cond 
        (contains? implied-mode opcode) 	 nil 
        (contains? immediate-mode opcode) lo
        (contains? relative-mode opcode) lo
        :else (mget c))))) 

(defn mget-bytes [c len] 
  (into [] (map (fn [x] (mem-peek c (+ (:address c) x))) (range len))))

(defn mget-word [c] (mem-peek-word c (:address c)))

(defn mput [c val]   (assoc (mem-poke c (:address c) val) :value (byteify val)))


(defn mload
  [c address bytes]
  (println address bytes)
  (reduce (fn [rc i] (mem-poke rc (+ address i) (get bytes i))) c (range (count bytes))))

;
; register operations
;
(defn rget [c register] (get c register))

;
; $PC has a range of $0000 to $FFFF all other registers have a range of $00 to $FF
;
(defn rput [c register val] 
  (let [val (if (not (= register :pc)) (byteify val) (bit-and val 0xffff))]
    (assoc c register val :value val)))
(defn radd [c register val] (rput c register (+ (rget c register) val)))
;
; manage the stack
;
(defn push [c val] (radd (assoc (assoc-in c [:mem (+ stack-base (rget c :sp))] (byteify val)) :value (byteify val)) :sp -1))
(defn pull [c reg] 
  (let [c (radd c :sp 1)]
    (rput c reg (get (:mem c) (+ stack-base (rget c :sp))))))
(defn push-word [c val] (push (push c (byteify (bit-shift-right val 8))) (byteify val)))
(defn pull-word [c reg] 
  (let [clo (pull c reg) chi (pull clo reg)] 
    (rput c reg (bit-or (rget clo reg) (bit-shift-left (rget chi reg) 8)))))
;
; flag management (status register :p)
;
(def flags {:c 0x01 :z 0x02 :i 0x04 :d 0x08 :b 0x10 :u 0x20 :v 0x40 :n 0x80})
(defn flag [f] (get flags f))
(defn flag-set? [c f] (> (bit-and (:p c) (flag f)) 0))
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

(defn set-address 
  "Set the address lines in the cpu"
  [c address]
  (assoc c :address address))

(defn get-relative 
  "Based on the negative flag, return v as -127 to 128 offset."
  [v] 
  (if (> (bit-and v (flag :n)) 0) (twos-complement v) v))

(defn page-boundary-check
  "for certain instructions, add a cycle if the page boundary was crossed"
  [c a1 a2]
  (if (not (= (bit-and a1 0xff) (bit-and a2 0xff))) (inc-cycles c) c))

(defn take-branch
  [c d]
  (let [a1 (rget c :pc) a2 (+ a1 (get-relative (mget c d)))]
  ;
  ; update the pc with the relative offset, add 1 cycle for a successful branch
  ; and add another cycle if it crossed a page boundary.
  ;
  (-> c (rput :pc a2) (inc-cycles) (page-boundary-check a1 a2))))
  

(defn absolute-indexed-mode-address
 [c op lo hi reg]
 (let [a1 (wordify lo hi) a2 (+ a1 (rget c reg))]
   [a2 (if (contains? page-boundary-opcodes op) (page-boundary-check c a1 a2) false)]))

(defn indirect-indexed-mode-address
  [c op lo hi]
  (let [a1 (mget-word c lo) a2 (+ a1 (rget c :y))]
    [a2 (if (contains? page-boundary-opcodes op) (page-boundary-check c a1 a2) false)]))


(fn [c op lo hi] [(+ (mget-word c lo) (rget c :y)) true])
  
(def address-mode-data
  { implied-mode    {:bytes 1 :address (fn [c op lo hi] [nil false]) :str (fn [op lo hi] (format "%s" (:name op)))}
		  immediate-mode  {:bytes 2 :address (fn [c op lo hi] [nil false]) :str (fn [op lo hi] (format "%s #$%02X" (:name op) lo))}
		  zeropage-mode   {:bytes 2 :address (fn [c op lo hi] [lo false]) :str (fn [op lo hi] (format "%s $%02X" (:name op) lo))}
		  relative-mode   {:bytes 2 :address (fn [c op lo hi] [lo false]) :str (fn [op lo hi] (format "%s $%02X" (:name op) lo))}
		  zeropagex-mode  {:bytes 2 :address (fn [c op lo hi] [(byteify (+ (rget c :x) lo)) false]) :str (fn [op lo hi] (format "%s $%02X,x" (:name op) lo))} 
		  zeropagey-mode  {:bytes 2 :address (fn [c op lo hi] [(byteify (+ (rget c :y) lo)) false]) :str (fn [op lo hi] (format "%s $%02X,y" (:name op) lo))}
		  absolute-mode   {:bytes 3 :address (fn [c op lo hi] [(wordify lo hi) false]) :str (fn [op lo hi] (format "%s $%04X" (:name op) (wordify lo hi)))} 
		  absolutex-mode  {:bytes 3 :address (fn [c op lo hi] (absolute-indexed-mode-address c op lo hi :x)) :str (fn [op lo hi] (format "%s $%04X,x" (:name op) (wordify lo hi)))} 
		  absolutey-mode  {:bytes 3 :address (fn [c op lo hi] (absolute-indexed-mode-address c op lo hi :y)) :str (fn [op lo hi] (format "%s $%04X,y" (:name op) (wordify lo hi)))} 
		  indirect-mode   {:bytes 3 :address (fn [c op lo hi] [(mget-word c (wordify lo hi)) false]) :str (fn [op lo hi] (format "%s $%04X" (:name op) (wordify lo hi)))} 
		  indexed-indirect-mode  {:bytes 2 :address (fn [c op lo hi] [(mget-word c (+ lo (rget c :x))) false]) :str (fn [op lo hi] (format "%s ($%02X,x)" (:name op) lo))}  
		  indirect-indexed-mode  {:bytes 2 :address indirect-indexed-mode-address :str (fn [op lo hi] (format "%s ($%02X),y" (:name op) lo))} 
  })

(defn get-address-mode-data 
   [op]
   (reduce  (fn [rmode mode] (
     if (contains? mode op) (get address-mode-data mode) rmode)) (get address-mode-data implied-mode) (keys address-mode-data)))

(defn set-address-from-mode
  "determines an address by determining the address mode of a given instruction"
  [c data]
  (let [[address cycles] ((:address (get-address-mode-data (get data 0))) c (get data 0) (get data 1) (get data 2))]
        (set-address (if cycles (inc-cycles c) c) address)))

(defn bittest
  [c d]

  (assoc (flag-if-has-flag (flag-if-has-flag c :n (mget c d)) :v (mget c d)) :value (bit-and (mget c d) (rget c :a))))



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
   {:name "TXS" :ops #{0x9a} :flags [:z :n] :fn (fn [c d] (rput c :sp (rget c :x)))}
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
   {:name "BCC" :ops #{0x90} :fn (fn [c d] (if (flag-clear? c :c) (take-branch c d) c))}
   {:name "BCS" :ops #{0xb0} :fn (fn [c d] (if (flag-set? c :c) (take-branch c d) c))}
   {:name "BEQ" :ops #{0xf0} :fn (fn [c d] (if (flag-set? c :z) (take-branch c d) c))}
   {:name "BNE" :ops #{0xd0} :fn (fn [c d] (if (flag-clear? c :z) (take-branch c d) c))}
   {:name "BMI" :ops #{0x30} :fn (fn [c d] (if (flag-set? c :n) (take-branch c d) c))}
   {:name "BPL" :ops #{0x10} :fn (fn [c d] (if (flag-clear? c :n) (take-branch c d) c))}
   {:name "BVC" :ops #{0x90} :fn (fn [c d] (if (flag-clear? c :v) (take-branch c d) c))}
   {:name "BVS" :ops #{0xb0} :fn (fn [c d] (if (flag-set? c :v) (take-branch c d) c))}
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
   {:name "CPY" :ops #{0xc0 0xc4 0xcc} :flags [:z :n] :fn (fn [c d] (let [comp (- (rget c :y) (mget c d))] (assoc (if (> comp 0) (fset c :c) c) :value comp)))}
   {:name "JSR" :ops #{0x20} :fn (fn [c d] (rput (push-word c (- (rget c :pc) 1)) :pc (:address c)))}
   {:name "RTS" :ops #{0x60} :fn (fn [c d] (let [c (pull-word c :pc)] (radd c :pc 1)))}])
     
(defn get-opcode [opcode]  (reduce (fn [rop op] (if (contains? (:ops op) opcode) op rop)) nil opcodes))

(defn fetch 
  "gets opcode and any data at the pc (or at an address alternatively)"
  ([c]
    (let [c (set-address c (rget c :pc)) opcode (mget c) len (:bytes (get-address-mode-data opcode))]
      (mget-bytes c len)))
  ([c address] 
    (let [c (set-address c address) opcode (mget c) len (:bytes (get-address-mode-data opcode))]
      (mget-bytes c len))))

(defn disassemble
  [c]
  (let [data (fetch c)]
    ((:str (get-address-mode-data (get data 0))) (get-opcode (get data 0)) (get data 1) (get data 2))))

(defn disassemble-at [c address] (disassemble (rput c :pc address)))

(defn show-cpu
  [c]
  (format "A: $%02X X: $%02X Y: $%02X PC: $%04X SP: $%02X P: $%02X Cycles: %d\n ________ \n|NV-BDIZC|\n|%s|\n --------\n%s" 
    (:a c) (:x c) (:y c) (:pc c) (:sp c) (:p c) (:cycles c)
    (reduce (fn [s d] (str s d)) "" (take-last 8 (concat (repeat 8 \0) (Integer/toBinaryString (:p c) ))))
    (format "$%04X: %s\n" (rget c :pc) (disassemble c))))


(defn exec
  "fetches op and data at pc and emulates instruction"
  [c]
  (let [data (fetch c) op (get-opcode (get data 0))]
   (-> c (radd :pc (count data)) 
       (set-address-from-mode data) (add-cycles-by-op data) ((:fn op) data) (set-flags (:flags op)))))


    ;(set-flags ((:fn op) c data) (:flags op))))

;
; TODO: The "stop on 0x00" is just a hack while bringing the full system online.
;
(def halt 0x00)
(defn run-cpu [c] (if (= halt (mem-peek c (rget c :pc))) c (recur (run-cpu (exec c)))))
