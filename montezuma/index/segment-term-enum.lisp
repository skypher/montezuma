(in-package #:montezuma)

(defclass segment-term-enum (term-enum)
  ((input :initarg :input)
   (field-infos :initarg :field-infos)
   (is-index :initarg :is-index)
   (position :initform -1 :reader pos)
   (term-buffer :initform (make-instance 'term-buffer))
   (prev-buffer :initform (make-instance 'term-buffer))
   (term-info :initform (make-instance 'term-info))
   (index-pointer :initform 0 :reader index-pointer)
   (format)
   (size :reader size)
   (index-interval :reader index-interval)
   (skip-interval :reader skip-interval)))

(defparameter +int-max+ (- (expt 2 31) 1))

(defmethod initialize-instance :after ((self segment-term-enum) &key)
  (with-slots (input format size is-index index-interval skip-interval
		     format-mlskip-interval) self
    (let ((first-int (read-int input)))
      (if (>= first-int 0)
	  ;; original-format file, without explicit format version number
	  (setf format 0
		size first-int
		index-interval 128
		skip-interval +int-max+)
	  (progn
	    (setf format first-int)
	    (when (< format +term-infos-format+)
	      (error "Unknown format version: ~S" format))
	    (setf size (read-long input))
	    (if (= format -1)
		(progn
		  (when (not is-index)
		    (setf index-interval (read-int input))
		    (setf format-mlskip-interval (read-int input)))
		  (setf skip-interval +int-max+))
		(setf index-interval (read-int input)
		      skip-interval (read-int input))))))))

(defmethod clone-object ((object segment-term-enum))
  (let ((copy (allocate-instance (class-of object))))
    (loop for slot in (class-slots (class-of object))
       do (setf (slot-value copy (slot-definition-name slot))
		(slot-value object (slot-definition-name slot))))
    copy))

(defmethod initialize-copy :after ((self segment-term-enum) other)
  (with-slots (input term-info term-buffer prev-buffer) self
    (setf input (clone (slot-value other 'input))
	  term-info (clone (slot-value other 'term-info))
	  term-buffer (clone (slot-value other 'term-buffer))
	  prev-buffer (clone (slot-value other 'prev-buffer)))))

(defmethod seek-segment-term ((self segment-term-enum) pointer position term term-info)
  (with-slots (input term-buffer prev-buffer) self
    (seek input pointer)
    (setf (slot-value self 'position) position)
    (setf (term term-buffer) term)
    (reset prev-buffer)
    (set-from-term-info (slot-value self 'term-info) term-info)))

(defmethod next ((self segment-term-enum))
  (with-slots (position field-infos term-buffer prev-buffer term-info format is-index index-pointer size
			format-mlskip-interval skip-interval input) self
    (incf position)
    (if (>= position size)
	(progn 
	  (reset term-buffer)
	  NIL)
	(progn
	  (set-from-term-buffer prev-buffer term-buffer)
	  (read-term-buffer term-buffer input field-infos)
	  (setf (doc-freq term-info) (read-vint input))
	  (incf (freq-pointer term-info) (read-vlong input))
	  (incf (prox-pointer term-info) (read-vlong input))
	  (if (= format -1)
	      (when (and (not is-index)
			 (> (doc-freq term-info) format-mlskip-interval))
		(setf (skip-offset term-info) (read-vint input)))
	      (when (>= (doc-freq term-info) skip-interval)
		(setf (skip-offset term-info) (read-vint input))))
	  (when is-index
	    (incf index-pointer (read-vlong input)))
	  T))))
	       

(defmethod scan-to ((self segment-term-enum) term)
  (with-slots (term-buffer) self
    (while (and (term> term (to-term term-buffer)) (next self)))))

(defmethod term ((self segment-term-enum))
  (with-slots (term-buffer) self
    (to-term term-buffer)))

(defmethod prev ((self segment-term-enum))
  (with-slots (prev-buffer) self
    (to-term prev-buffer)))

(defmethod term-info ((self segment-term-enum))
  (with-slots (term-info) self
    (clone term-info)))

(defmethod (setf term-info) (term-info (self segment-term-enum))
  (setf (slot-value self 'term-info) term-info))

(defmethod doc-freq ((self segment-term-enum))
  (with-slots (term-info) self
    (doc-freq term-info)))

(defmethod freq-pointer ((self segment-term-enum))
  (with-slots (term-info) self
    (freq-pointer term-info)))

(defmethod prox-pointer ((self segment-term-enum))
  (with-slots (term-info) self
    (freq-pointer term-info)))

(defmethod close ((self segment-term-enum))
  (with-slots (input) self
    (close input)))
