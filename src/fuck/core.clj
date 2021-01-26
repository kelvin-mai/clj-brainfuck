(ns fuck.core)

(defn initialize [memory-size]
  {:program nil
   :memory (vec (take memory-size (repeat 0)))
   :data-ptr 0
   :instruction-ptr 0
   :operations 0
   :while-stack (list)})

(defn load-program [state program] (assoc state :program program))
(defn instruction-pointer-inc [state] (update-in state [:instruction-ptr] inc))
(defn instruction-pointer-reset [state] (assoc state :instruction-ptr (first (:while-stack state))))
(defn data-pointer-inc [state] (update-in state [:data-ptr] inc))
(defn data-pointer-dec [state] (update-in state [:data-ptr] dec))
(defn value-inc [state] (update-in state [:memory (:data-ptr state)] inc))
(defn value-dec [state] (update-in state [:memory (:data-ptr state)] dec))
(defn value [state] (get-in state [:memory (:data-ptr state)]))
(defn instruction [state] (get-in state [:program (:instruction-ptr state)]))
(defn output [state]
  (print (char (get-in state [:memory (:data-ptr state)]))) state)
(defn input [state]
  state)

(defn move-while-close [state]
  (if (= (instruction state) \]) state (recur (instruction-pointer-inc state))))

(defn while-open [state]
  (if (= (value state) 0)
    (move-while-close state) (update-in state [:while-stack] conj (:instruction-ptr state))))

(defn while-close [state]
  (if (not= (value state) 0) (instruction-pointer-reset state) (update-in state [:while-stack] rest)))

(def operation-map
  {\> data-pointer-inc
   \< data-pointer-dec
   \+ value-inc
   \- value-dec
   \. output
   \, input
   \[ while-open
   \] while-close})

(defn execute-command [state]
  (let [current-instruction (instruction state)
        run-fn (operation-map current-instruction)]
    (if (not (fn? run-fn))
      state
      (run-fn state))))

(defn step [state]
  (-> state
      (execute-command)
      (instruction-pointer-inc)
      (update-in [:operations] inc)))

(defn run [state]
  (let [current-instruction (instruction state)]
    (if (nil? current-instruction)
      {:status "complete" :operations (:operations state)}
      (recur (step state)))))

(def HELLO-WORLD "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

(def HELLO-WORLD-2 "+++++ +++++             initialize counter (cell #0) to 10
                   [                       use loop to set 70/100/30/10
                   > +++++ ++              add  7 to cell #1
                   > +++++ +++++           add 10 to cell #2
                   > +++                   add  3 to cell #3
                   > +                     add  1 to cell #4
                   <<<< -                  decrement counter (cell #0)
                   ]
                   > ++ .                  print 'H'
                   > + .                   print 'e'
                   +++++ ++ .              print 'l'
                   .                       print 'l'
                   +++ .                   print 'o'
                   > ++ .                  print ' '
                   << +++++ +++++ +++++ .  print 'W'
                   > .                     print 'o'
                   +++ .                   print 'r'
                   ----- - .               print 'l'
                   ----- --- .             print 'd'
                   > + .                   print '!'
                   > .                     print '\n'")

(defn -main []
  (-> (initialize 8)
      (load-program HELLO-WORLD-2)
      (run)))

