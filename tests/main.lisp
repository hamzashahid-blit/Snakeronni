(defpackage snakeronni/tests/main
  (:use :cl
        :snakeronni
        :rove))
(in-package :snakeronni/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :snakeronni)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
