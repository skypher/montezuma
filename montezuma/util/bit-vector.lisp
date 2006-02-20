(in-package #:montezuma)


(defun make-bit-vector ()
  (list 0))

(defun bit-set-p (bit-vector index)
  (logbitp index (car bit-vector)))

(defun set-bit (bit-vector index)
  (setf (ldb (byte 1 index) (car bit-vector)) 1))

(defun clear-bit (bit-vector index)
  (setf (ldb (byte 1 index) (car bit-vector)) 0))

(defun bit-vector-count (bit-vector)
  (logcount (car bit-vector)))