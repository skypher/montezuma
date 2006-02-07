(in-package #:montezuma)

(defparameter +term-infos-format+ -2)

(defclass term-infos-writer ()
  ((index-interval :initarg :interval)
   (skip-interval :initform 16)
   (last-index-pointer :initform 0)
   (last-term :initform (make-term "" ""))
   (last-term-info :initform (make-instance 'term-info))
   (size :initform 0 :reader size)
   (is-index :initarg :is-index)
   (field-infos :initarg :field-infos)
   (out :reader out)
   (other :accessor other))
  (:default-initargs
   :is-index NIL))

(defmethod initialize-instance :after ((self term-infos-writer) &key directory segment)
  (with-slots (out is-index index-interval skip-interval) self
    (setf out (create-output directory (add-file-extension
					segment
					(if is-index "tii" "tis"))))
    (write-int out +term-infos-format+)
    (write-long out 0)
    (write-int out index-interval)
    (write-int out skip-interval)
    (unless is-index
      (with-slots (other field-infos index-interval is-index) self
	(setf other (make-instance 'term-infos-writer
				   :directory directory
				   :segment segment
				   :field-infos field-infos
				   :interval index-interval
				   :is-index T))
	(setf (other other) self)))))

(defmethod add-term ((self term-infos-writer) term term-info)
  (with-slots (is-index last-term last-term-info index-interval last-index-pointer skip-interval size other) self
    (when (and (not is-index) (term> last-term term))
      (error "term out of order: ~S < ~S" term last-term))
    (when (< (freq-pointer term-info) (freq-pointer last-term-info))
      (error "freq pointer out of order: ~S < ~S."
	     (freq-pointer term-info)
	     (freq-pointer last-term-info)))
    (when (and (not is-index) (zerop (mod size index-interval)))
      (add-term other last-term last-term-info))
    (with-slots (out) self
      (write-term self term)
      (write-vint out (doc-freq term-info))
      (write-vlong out (- (freq-pointer term-info) (freq-pointer last-term-info)))
      (write-vlong out (- (prox-pointer term-info) (prox-pointer last-term-info)))
      (when (>= (doc-freq term-info) skip-interval)
	(write-vint out (skip-offset term-info)))
      (when is-index
	(write-vlong out (- (pos (out other)) last-index-pointer))
	(setf last-index-pointer (pos (out other))))
      (set-from-term-info last-term-info term-info)
      (incf size))))

(defmethod close ((self term-infos-writer))
  (with-slots (out size other is-index) self
    (seek out 4)
    (write-long out size)
    (close out)
    (unless is-index
      (close other))))

(defmethod write-term ((self term-infos-writer) term)
  (with-slots (out field-infos last-term) self
    (let* ((start (or (mismatch (term-text last-term) (term-text term))
		      (min (length (term-text last-term)) (length (term-text term)))))
	   (length (- (length (term-text term)) start)))
      (write-vint out start)
      (write-vint out length)
      (write-chars out (string-to-bytes (term-text term)) start length)
      (write-vint out (get-field-number field-infos (term-field term)))
      (setf last-term term))))



(defclass term-infos-reader ()
  ((directory :initarg :directory)
   (segment :initarg :segment)
   (field-infos :initarg :field-infos)
   (orig-enum)
   (size :reader size)
   (skip-interval :reader skip-interval)
   (index-enum)
   (index-terms :initform nil)
   (index-infos :initform nil)
   (index-pointers :initform nil)
   (cached-term-enum :initform nil)))

(defmethod initialize-instance :after ((self term-infos-reader) &key)
  (with-slots (orig-enum size directory segment field-infos index-enum skip-interval) self
    (setf orig-enum (make-instance 'segment-term-enum
				   :input (open-input directory
						      (format nil "~A.tis" segment))
				   :field-infos field-infos
				   :is-index NIL))
    (setf size (size orig-enum))
    (setf skip-interval (skip-interval orig-enum))
    (setf index-enum (make-instance 'segment-term-enum
				    :input (open-input directory
						       (format nil "~A.tii" segment))
				    :field-infos field-infos
				    :is-index T))))

(defmethod close ((self term-infos-reader))
  (with-slots (orig-enum index-enum) self
    (when orig-enum
      (close orig-enum))
    (when index-enum
      (close index-enum))))

(defmethod get-term-info ((self term-infos-reader) term)
  (with-slots (size index-terms) self
    (if (= size 0)
	nil
	(progn
	  (ensure-index-is-read self)
	  (let ((e (enum self)))
	    (when (and (term e) (term> term (term e)))
	      (let ((enum-offset (+ (floor (pos e) (index-interval e)) 1)))
		(when (or (= (length index-terms) enum-offset)
			  (term< term (aref index-terms enum-offset)))
		  (return-from get-term-info (scan-for-term-info self term))))))
	  (seek-enum self (get-index-offset self term))
	  (scan-for-term-info self term)))))

(defmethod get-term ((self term-infos-reader) position)
  (with-slots (size) self
    (if (= size 0)
	nil
	(let ((e (enum self)))
	  (if (and (not (null e))
		   (>= position (pos e))
		   (< position (+ (pos e) (index-interval e))))
	      (scan-for-term self position)
	      (progn
		(seek-enum self (floor position (index-interval e)))
		(scan-for-term self position)))))))

(defmethod get-terms-position ((self term-infos-reader) term)
  (with-slots (size) self
    (if (= size 0)
	nil
	(progn
	  (ensure-index-is-read self)
	  (seek-enum self (get-index-offset self term))
	  (let ((e (enum self)))
	    (while (and (term> term (term e))
			(next e)))
	    (if (and (term e) (term= term (term e)))
		(pos e)
		-1))))))

(defmethod terms ((self term-infos-reader))
  (with-slots (orig-enum) self
    (clone orig-enum)))

(defmethod terms-from ((self term-infos-reader) term)
  (get-term-info self term)
  (clone (enum self)))

(defmethod enum ((self term-infos-reader))
  ;; FIXME use cached thread-local storage?
  (with-slots (cached-term-enum) self
    (when (null cached-term-enum)
      (setf cached-term-enum (terms self)))
    cached-term-enum))

(defmethod ensure-index-is-read ((self term-infos-reader))
  ;; FIXME synchronized?
  (with-slots (index-terms index-enum index-infos index-pointers) self
    (unless index-terms
      (unwind-protect
	   (let ((index-size (size index-enum)))
	     (setf index-terms (make-array index-size))
	     (setf index-infos (make-array index-size))
	     (setf index-pointers (make-array index-size))
	     (do ((i 0 (+ i 1)))
		 ((not (next index-enum)))
	       (setf (aref index-terms i) (term index-enum))
	       (setf (aref index-infos i) (term-info index-enum))
	       (setf (aref index-pointers i) (index-pointer index-enum))))
	(close index-enum)
	(setf index-enum nil)))))

(defmethod get-index-offset ((self term-infos-reader) term)
  (with-slots (index-terms index-infos index-pointers) self
    (let ((lo 0)
	  (hi (- (length index-terms) 1)))
      (while (>= hi lo)
	(let* ((mid (floor (+ lo hi) 2))
	       (delta (term-compare term (aref index-terms mid))))
	  (cond ((< delta 0)
		 (setf hi (- mid 1)))
		((> delta 0)
		 (setf lo (+ mid 1)))
		(T
		 (return-from get-index-offset mid)))))
      hi)))

(defmethod seek-enum ((self term-infos-reader) ind-offset)
  (with-slots (index-pointers index-terms index-infos) self
    (seek-segment-term (enum self)
		       (aref index-pointers ind-offset)
		       (- (* ind-offset (index-interval (enum self))) 1)
		       (aref index-terms ind-offset)
		       (aref index-infos ind-offset))))

(defmethod scan-for-term-info ((self term-infos-reader) term)
  (let ((e (enum self)))
    (scan-to e term)
    (if (and (term e) (term= term (term e)))
	(term-info e)
	nil)))

(defmethod scan-for-term ((self term-infos-reader) position)
  (let ((e (enum self)))
    (while (< (pos e) position)
      (unless (next e)
	(return-from scan-for-term nil)))
    (term e)))

(defmethod get-position (self term)
  (with-slots (size) self
    (if (= size 0)
	-1
	(let ((ind-offset (get-index-offset term)))
	  (seek-enum self ind-offset)
	  (let ((e (enum self)))
	    (while (and (term> term (term e))
			(next e)))
	    (if (term= term (term e))
		(pos e)
		-1))))))
