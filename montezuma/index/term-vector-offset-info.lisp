(in-package #:montezuma)

(defclass term-vector-offset-info ()
  ((start-offset :initarg :start-offset :reader start-offset)
   (end-offset :initarg :end-offset :reader end-offset)))

(defmethod initialize-instance :after ((self term-vector-offset-info) &key)
  (assert (<= (start-offset self) (end-offset self))))

(defmethod print-object ((self term-vector-offset-info) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "start: ~S  end: ~S"
	    (start-offset self)
	    (end-offset self))))

(defun term-vector-offset-info= (a b)
  (and (typep a 'term-vector-offset-info) (typep b 'term-vector-offset-info)
       (= (start-offset a) (start-offset b))
       (= (end-offset a) (end-offset b))))


