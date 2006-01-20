(in-package #:montezuma)

(defun similarity-byte-to-float (b)
  ;; FIXME
  b)
	
(defun similarity-float-to-byte (f)
  ;; FIXME
  f)

(defclass similarity ()
  ())

(defclass default-similarity (similarity)
  ())

(defun make-default-similarity ()
  (make-instance 'default-similarity))
