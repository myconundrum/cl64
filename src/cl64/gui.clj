(ns cl64.gui
  (:require [cl64.memory :refer :all]
 											[cl64.computer :refer :all]
 											[cl64.cpu :refer :all]
 											[cl64.c64 :refer :all]
 											[clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer poll! take! sliding-buffer close! thread
                     alts! alts!! timeout]]
 											[seesaw.core :as seesaw]
 											))


(def window-height 480)
(def window-width 650)


(defn display
  [content]
  (-> (seesaw/frame :title "Commodore-64 Emulator")
       (seesaw/config! :content content)
       (seesaw/pack!)
       (seesaw/show!)))




(defn make-ui
  []
  {:out (chan (sliding-buffer 3))})
  


(def ui-events #{:new-value})


(defn gui-get-event
  [ui]
  (poll! (:out ui)))


(defn get-ui-value
  [ui name]
  (seesaw/text (get ui name)))

(defn textfield [ui name txt editable]

   (let [t (seesaw/text
         :text txt
         :editable? editable
         :columns 50)
         ui (assoc ui name t)]

     (seesaw/listen t
       :key-pressed (fn [e] 
         (when (= (.getKeyChar e) \newline) (>!! (:out ui) [:new-value name]))))

      ui))

(defn top-bottom [top bottom]
  (seesaw/top-bottom-split top bottom))




(def window

 (seesaw/frame
 :title "Commodore 64 Emulator"
 :content "Hello\nWorld!"
 :width window-width
 :height window-height))

(defn gui-init 
  []

  (seesaw/native!)
  (let [ui (-> (make-ui) (textfield "top" "this is the top" true) (textfield "bottom" "bottom is editable" true))]
     (display (top-bottom (get ui "top") (get ui "bottom"))) ui))







