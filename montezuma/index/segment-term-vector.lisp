(in-package #:montezuma)

(defclass segment-term-vector ()
  ((field :initarg :field :reader field)
   (terms :initarg :terms :reader terms)
   (term-frequencies :initarg :term-frequencies :reader term-frequencies)
   (positions :initarg :positions :reader positions)
   (offsets :initarg :offsets :reader offsets)))

(defmethod print-object ((self segment-term-vector) stream)
  (print-unreadable-object (self stream :identity T :type T)
    (with-slots (field terms term-frequencies positions offsets) self
      (format stream "field:~S terms:~S term-freqs:~S positions:~S offsets:~S"
	      field terms term-frequencies positions offsets))))

(defmethod size ((self segment-term-vector))
  (with-slots (terms) self
    (length terms)))

(defmethod index-of ((self segment-term-vector) term)
  (with-slots (terms) self
    (position term terms :test #'string=)))

(defmethod indexes-of ((self segment-term-vector) terms start len)
  (mapcar #'(lambda (term)
	      (index-of self term))
	  (subseq terms start (+ start len))))
