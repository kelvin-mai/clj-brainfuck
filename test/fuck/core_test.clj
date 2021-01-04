(ns fuck.core-test
  (:require [clojure.test :refer :all]
            [fuck.core :refer [-main]]))

(def HELLO-WORLD "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

(deftest test-fuck
  (testing "Test HELLO-WORLD"
    (is (= "HELLO-WORLD!"
           (-main HELLO-WORLD)))))
