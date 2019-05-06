(ns cl64.core-test
  (:require [clojure.test :refer :all]
            [cl64.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest test-gets-puts
  (let [cpu (make-cpu) memory (make-memory memory-size)]
    (is (= (get-reg (put-reg cpu :x (get-byte (put-byte memory 100 50) 100)) :x)) 50)
    (is (= (get-word (put-byte (put-byte memory 101 0xDE) 100 0xAD) 100) 0xDEAD))
    (is (= (get-word (put-word memory 100 1234) 100) 1234))
    (is (= (get-byte (put-word memory 100 0xDEAD) 100) 0xAD))
    (is (= (get-byte (put-word memory 100 0xDEAD) 101) 0xDE))
    (is (= (get-bytes (put-word memory 100 0xDEAD) 100 3) [0xAD 0xDE 0x00]))
    (is (= (get-byte-at-pc cpu (put-byte memory (get-reg cpu :pc) 0x20)) 0x20))
    (is (= (get-reg (inc-pc cpu) :pc) 1))
    (is (= (get-reg (add-reg cpu :a 5) :a) 5))
    (is (= (get-reg (sub-reg (add-reg cpu :a 5) :a 3) :a) 2))))
