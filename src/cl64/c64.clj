(ns cl64.c64
  (:require [cl64.computer :refer :all]
  										[cl64.memory :refer :all]
												[cl64.cpu :refer :all]
  										))




;
; processor port determines what memory is banked in vs ROMs as well
; as some peripheral information.
; address $00 serves as a mask of what bits are writeable in the address $01. (bankswitch)
;
(def processor-port-address 0x0000)
(def bankswitch-address 0x0001)
(def processor-port-address-end 0x0002)
(def bankswitch-default-value 0x37)
(def processor-port-default-value 0x2F)

(def basic-rom-start 0xa000)
(def kernal-rom-start 0xe000)
;
; manage standard rom pokes/peeks when mapped in.
;
(defn peek-rom [c mmap address] (get (:data mmap) (- address (:from mmap))))
(defn poke-rom [c mmap address val] (println (format "Warning! poke attempted in rom at $%04X" address)) c)

(def c64-roms
  [{:name "basic" :path "bin/basic.bin" :start basic-rom-start}
   {:name "kernal" :path "bin/kernal.bin" :start kernal-rom-start}])

;
; the bankswitch address (0x01 in zeropage memory) determines which roms are mapped into memory and other 
; behaviour.  See http://sta.c64.org/cbm64mem.html for details on bankswitching behaviour bits. 
(defn bankswitch-update
  [c]
  (let [val (bit-and (mem-unmapped-peek c bankswitch-address) 0x7)
    active [{:name "basic"  :active (= (bit-and val 0x3) 0x3)}
            {:name "kernal" :active (> (bit-and val 0x2) 0)}]]
            ;"charrom" (and (> val 0) (= (bit-and 0x4) 0))
            ;"io" (and (> val 0) (> (bit-and 0x4) 0))
    (println val)
    (reduce (fn [rc am] (set-memory-map-active rc (:name am) (:active am))) c active)))

(defn processor-port-poke
 [c mmap address val]
 (println (format "Processor port poke detected at address $%04X with poke value of $%02X." address val))
 (if (= address bankswitch-address)
    (bankswitch-update (mem-unmapped-poke c address (bit-and (mem-peek c processor-port-address) val)))
    (mem-unmapped-poke c address val)))

(defn set-bankswitch
  [c]
  (bankswitch-update (mem-unmapped-poke (mem-unmapped-poke (add-memory-map c 
    {:name "processor port" :from processor-port-address :to processor-port-address-end
    	:peekfn mem-unmapped-peek :pokefn processor-port-poke :active true :data []}) 
    processor-port-address processor-port-default-value) 
    bankswitch-address bankswitch-default-value)))

(defn load-c64-roms
  [c]
  (reduce (fn [fc rom] (let [data (slurp-bytes (:path rom))]
    (add-memory-map fc 
      {:name (:name rom) :from (:start rom) :to (+ (count data) (:start rom)) 
      	:peekfn peek-rom :pokefn poke-rom :active false :data data}))) c c64-roms))

(defn make-c64 []
  (set-bankswitch(      ; set bankswitch monitor and default value
    load-c64-roms (     ; load roms
      make-computer)))) ; initialize basic computer
  


