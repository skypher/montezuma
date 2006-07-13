(in-package #:montezuma)

(declaim (inline length-1-list-p))
(defun length-1-list-p (x) 
  "Is x a list of length 1? Note that this is better than the naive \(= \(length x\) 1\) because we don't need to traverse the entire list..."
  (and (consp x) (null (cdr x))))

;;; ---------------------------------------------------------------------------
;;; each method
;;; ---------------------------------------------------------------------------

(defgeneric each (sequence function))

(defmethod each ((self list) block)
  (mapc block self))


(defun parse-float (string)
  (with-standard-io-syntax
    (read-from-string string)))
