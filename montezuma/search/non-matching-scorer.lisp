(in-package #:montezuma)

;; A scorer that matches no document at all.

(defclass non-matching-scorer (scorer)
  ())

(defmethod next? ((self non-matching-scorer))
  NIL)

(defmethod skip-to ((self non-matching-scorer) target)
  (declare (ignore target))
  NIL)

