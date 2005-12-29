(in-package #:montezuma)

(defclass term-vector-offset-info ()
  ((start-offset :initarg :start-offset :reader start-offset)
   (end-offset :initarg :end-offset :reader end-offset)))


(defun term-vector-offset-info= (a b)
  (and (typep a 'term-vector-offset-info) (typep b 'term-vector-offset-info)
       (= (start-offset a) (start-offset b))
       (= (end-offset a) (end-offset b))))


