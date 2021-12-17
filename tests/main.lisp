(defpackage mcnibbles/tests/main
  (:use :cl
        :mcnibbles
        :rove))
(in-package :mcnibbles/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :mcnibbles)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
