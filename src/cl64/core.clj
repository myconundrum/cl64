(ns cl64.core
  (:gen-class)
  (:require [cl64.memory :refer :all]
 											[cl64.computer :refer :all]
 											[cl64.cpu :refer :all]
 											[clojure.java.io :as io]))

(require '[clojure.test :refer :all])
(require '[clojure.string :as str])

(defn make-session [] {:computer (make-computer) :running true})

(defn str-to-num [#^String s]
  (binding [*read-eval* false]
    (let [n (read-string s)]
      (when (number? n)
        n))))

;
; TODO: naive implementation
;
(defn eval-number-string-force-hex
  [string]
  (str-to-num (str "0x" string)))

;
; TODO: naive implementation.
;
(defn eval-number-string
  [string]
  (if (= (first string) \$) 
    (str-to-num (str "0x" (subs string 1))) 
    (str-to-num string)))

(defn handle-lb-cmd
  [session in]
  ["Loading bytes into memory."
  (assoc session :computer 
    (mload (:computer session) (eval-number-string (get in 1 "0"))
      (map eval-number-string (rest (rest in)))))])

(defn load-hex-line
  [c line]
  ;
  ; TODO: Need to check checksum and validate file. Super simple right now.
  ;
  (let [header (subs line 0 9) 
        body (subs line 9)
        len (eval-number-string-force-hex (subs header 1 3))
        address (eval-number-string-force-hex (subs header 3 7))
        bytes (into [] (map (fn [v] (eval-number-string-force-hex (subs body (* v 2) (+ (* v 2) 2)))) (range len)))]
    (reduce (fn [rc i] (mpoke rc (+ address i) (get bytes i))) c (range len))))


(defn file-exists? [path] (and (not (.isDirectory (io/file path))) (.exists (io/file path))))
(defn handle-load-cmd
  [session in]
  (let [base (get in 1 "")
        path (cond 
               (file-exists? base) base
               (file-exists? (format "asm/%s" base)) (format "asm/%s" base)
               (file-exists? (format "asm/%s.a65.hex" base)) (format "asm/%s.a65.hex" base)
               :else nil)
        data (if path (str/split-lines (slurp path)) nil)]
    (if path [(format "Found %d lines of data in %s." (count data) path)
    										(assoc session :computer (reduce load-hex-line (:computer session) data))]
    										["File not found." session])))   


(def reg-map {"pc" :pc "a" :a "x" :x "y" :y "p" :p "sp" :sp})
(defn handle-set-cmd
  [session in]
  ["Setting register."
   (assoc session :computer (rput (:computer session) (get reg-map (get in 1 "pc")) (eval-number-string (get in 2 "0"))))])

(def commands {
	 "quit"  {:fn (fn [s d]  ["Goodbye." (assoc s :running false)]) 
	       :help "Quits interactive shell."}
  "lb" {:fn handle-lb-cmd 
  	     :help "Load Bytes.\nUsage: lb <addr> <bytes>\nLoad <bytes> into memory at <addr>."}
  "db" {:fn (fn [s d] [(dump-page (:computer s) (eval-number-string (get d 1 "0"))) s]) 
  	     :help "Dump Bytes.\nUsage: db <addr>\n Dumps a page of memory starting at <addr>."}
  "show" {:fn (fn [s d] [(show-computer (:computer s)) s])
  	     :help "Show computer state."}
  "help" {:fn (fn [s d] [(reduce (fn [rs h] (format "%s\n[%s]\n%s" rs (key h) (get (get commands (key h)) :help))) "" commands) s])
        :help "Show this message."}
  "step" {:fn (fn [s d] (let [c (exec (:computer s))] [
  	         (format "after %s \n%s" (disassemble (:computer s)) (show-computer c))
  	         (assoc s :computer c)]))
  	     :help "Execute instruction at current pc register."}
  "load" {:fn handle-load-cmd 
  	     :help "Load Hex File.\nUsage: load <path>\nLoads the file into memory."}
  "run" {:fn (fn [s d] ["Running to halt." (assoc s :computer (run-cpu (:computer s)))])
        :help "Run until halt."}

  "set" {:fn handle-set-cmd
         :help "Set register.\nUsage: set <reg> <value>"}
  	     })

(defn handle-command 
  [session]
  (println "Ready.")
  (let [in (str/split (str/lower-case (read-line)) #" ") func (get commands (get in 0))]
    (if (not func)
      (do (println "Unknown command.") session)
      (let [results ((get func :fn) session in)]
        (println (get results 0))
        (get results 1)))))

(defn start-interactive
  []
  (println "Starting 6502 interactive shell.")
  (loop [session (make-session)]
    (when (:running session)
      (recur (handle-command session)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (start-interactive))

