(in-package #:montezuma)

(defclass req-excl-scorer (scorer)
  ((req-scorer :initarg :req-scorer)
   (excl-scorer :initarg :excl-scorer)
   (first-time-p :initform T)))

(defmethod next? ((self req-excl-scorer))
  (with-slots (first-time-p excl-scorer req-scorer) self
    (when first-time-p
      (when (not (next? excl-scorer))
	(setf excl-scorer nil))
      (setf first-time-p NIL))
    (cond ((null req-scorer)
	   nil)
	  ((not (next? req-scorer))
	   (setf req-scorer nil)
	   nil)
	  ((null excl-scorer)
	   T)
	  (T
	   (to-non-excluded self)))))

(defgeneric to-non-excluded (req-excl-scorer))

(defmethod to-non-excluded ((self req-excl-scorer))
  (with-slots (excl-scorer req-scorer) self
    (let ((excl-doc (document excl-scorer)))
      (loop do
	   (let ((req-doc (document req-scorer)))
	     (cond ((< req-doc excl-doc)
		    (return-from to-non-excluded T))
		   ((> req-doc excl-doc)
		    (unless (skip-to excl-scorer req-doc)
		      (setf excl-scorer nil)
		      (return-from to-non-excluded T))
		    (setf excl-doc (document excl-scorer))
		    (when (> excl-doc req-doc)
		      (return-from to-non-excluded T)))))
	   while (next? req-scorer))
      (setf req-scorer nil)
      nil)))

(defmethod document ((self req-excl-scorer))
  (document (slot-value self 'req-scorer)))

(defmethod score ((self req-excl-scorer))
  (score (slot-value self 'req-scorer)))

(defmethod skip-to ((self req-excl-scorer) target)
  (with-slots (first-time-p excl-scorer req-scorer) self
    (when first-time-p
      (setf first-time-p NIL)
      (when (not (skip-to excl-scorer target))
	(setf excl-scorer nil)))
    (cond ((null req-scorer)
	   nil)
	  ((null excl-scorer)
	   (skip-to req-scorer target))
	  ((not (skip-to req-scorer target))
	   (setf req-scorer nil)
	   nil)
	  (T
	   (to-non-excluded self)))))
