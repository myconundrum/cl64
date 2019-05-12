(ns cl64.core
  (:gen-class)
  (:require [cl64.memory :refer :all]
 											[cl64.computer :refer :all]
 											[cl64.cpu :refer :all]))

(require '[clojure.test :refer :all])
(require '[clojure.string :as str])

(defn make-session [] {:computer (make-computer) :running true})

(defn str-to-num [#^String s]
  (binding [*read-eval* false]
    (let [n (read-string s)]
      (when (number? n)
        n))))

(defn eval-number-string
  [string]
  (if (= (first string) \$) 
    (str-to-num (str "0x" (subs string 1))) 
    (str-to-num string)))

(defn handle-lb-cmd
  [session in]
  ["Loading bytes into memory."
  (assoc session :computer 
    (mload (:computer session) (eval-number-string (get in 1))
      (map eval-number-string (rest (rest in)))))])

(def commands {
	 "quit"  {:fn (fn [s d]  ["Goodbye." (assoc s :running false)]) :help "Quits interactive shell."}
  "lb" {:fn handle-lb-cmd 
  	     :help "Load Bytes.\nUsage: lb <addr> <bytes>\nLoad <bytes> into memory at <addr>."}
  "db" {:fn (fn [s d] [(dump-page (:computer s) (eval-number-string (get d 1))) s]) 
  	     :help "Dump Bytes.\nUsage: db <addr>\n Dumps a page of memory starting at <addr>."}
  "show" {:fn (fn [s d] [(show-computer (:computer s)) s])
  	     :help "Show computer state."}
  "help" {:fn (fn [s d] [(reduce (fn [rs h] (format "%s\n[%s]\n%s" rs (key h) (get (get commands (key h)) :help))) "" commands) s])
        :help "Show this message."}
  "step" {:fn (fn [s d] (let [c (exec (:computer s))] [(show-computer c) (assoc s :computer c)]))
  	     :help "Execute instruction at current pc register."}
  "run" {:fn (fn [s d] ["Running to halt." (assoc s :computer (run-cpu (:computer s)))])
        :help "Run until halt."}
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

