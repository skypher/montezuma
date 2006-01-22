(in-package #:montezuma)

(defun similarity-byte-to-float (b)
  ;; FIXME
  1.0)
	
(defun similarity-float-to-byte (f)
  ;; FIXME
  1)


(defparameter *norm-table* (make-array 256
				       :initial-contents (loop for i from 0 below 256
							    collecting (similarity-byte-to-float i))))

(defun decode-norm (b)
  (aref *norm-table* (logand b #xff)))

(defun encode-norm (f)
  (similarity-float-to-byte f))

(defclass similarity ()
  ())

(defclass default-similarity (similarity)
  ())

(defun make-default-similarity ()
  (make-instance 'default-similarity))


(defmethod length-norm ((self default-similarity) field num-terms)
  (declare (ignore field))
  (/ 1.0 (sqrt num-terms)))

(defmethod query-norm ((self default-similarity) sum-of-squared-weights)
  (/ 1.0 (sqrt sum-of-squared-weights)))

(defmethod tf ((self default-similarity) freq)
  (sqrt freq))
