(in-package #:montezuma)

(defclass term-info ()
  ((doc-freq     :initarg :doc-freq     :accessor doc-freq)
   (freq-pointer :initarg :freq-pointer :accessor freq-pointer)
   (prox-pointer :initarg :prox-pointer :accessor prox-pointer)
   (skip-offset  :initarg :skip-offset  :accessor skip-offset))
  (:default-initargs
   :doc-freq 0
   :freq-pointer 0
   :prox-pointer 0
   :skip-offset 0))

(defmethod print-object ((self term-info) stream)
  (print-unreadable-object (self stream :identity T :type T)
    (with-slots (doc-freq freq-pointer prox-pointer skip-offset) self
      (format stream "df=~S:fp=~S:pp=~S:so=~S"
	      doc-freq
	      freq-pointer
	      prox-pointer
	      skip-offset))))

(defmethod initialize-copy :after ((self term-info) other)
  (with-slots (doc-freq freq-pointer prox-pointer skip-offset) self
    (setf doc-freq (doc-freq other)
	  freq-pointer (freq-pointer other)
	  prox-pointer (prox-pointer other)
	  skip-offset (skip-offset other))))
	    
					       
(defmethod set-from-term-info ((self term-info) ti)
  (with-slots (doc-freq freq-pointer prox-pointer skip-offset) self
    (setf doc-freq (doc-freq ti)
	  freq-pointer (freq-pointer ti)
	  prox-pointer (prox-pointer ti)
	  skip-offset (skip-offset ti))
    self))

(defun term-info= (ti1 ti2)
  (and (typep ti1 'term-info) (typep ti2 'term-info)
       (= (doc-freq ti1) (doc-freq ti2))
       (= (freq-pointer ti1) (freq-pointer ti2))
       (= (prox-pointer ti1) (prox-pointer ti2))
       (= (skip-offset ti1) (skip-offset ti2))))

