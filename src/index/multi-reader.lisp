(in-package #:montezuma)

(defclass multi-reader (index-reader)
  ((sub-readers :initarg :sub-readers)
   (max-doc :initform 0 :reader max-doc)
   (num-docs :initform -1)
   (starts)
   (ones)
   (norms-cache :initform (make-table :test #'equal))
   (has-deletions-p :initform NIL)))
   

(defmacro with-sub-reader ((sub-reader-var start-var) multi-reader n &body body)
  (with-gensyms (index-var)
    (once-only (multi-reader n)
      `(let* ((,index-var (reader-index ,multi-reader ,n))
	      (,sub-reader-var (aref (slot-value self 'sub-readers) ,index-var))
	      (,start-var (- ,n (aref (slot-value self 'starts) ,index-var))))
	 ,@body))))

	 

(defmethod initialize-instance :around ((self multi-reader) &key directory sub-readers)
  (if directory
      (call-next-method)
      (call-next-method
       :directory (if (= (length sub-readers) 0)
		      nil
		      (directory (aref sub-readers 0)))))
  (with-slots (starts max-doc has-deletions-p) self
    (setf starts (make-array (+ (length sub-readers) 1)))
    (dosequence (sub-reader sub-readers :index i)
      (setf (aref starts i) max-doc)
      (incf max-doc (max-doc sub-reader))
      (if (has-deletions-p (aref sub-readers i))
	  (setf has-deletions-p T)))
    (setf (aref starts (length sub-readers)) max-doc)))

(defmethod get-term-vectors ((self multi-reader) n)
  (with-sub-reader (sub-reader start) self n
    (get-term-vectors sub-reader start)))

(defmethod get-term-vector ((self multi-reader) n field)
  (with-sub-reader (sub-reader start) self n
    (get-term-vector sub-reader start field)))

(defmethod num-docs ((self multi-reader))
  (with-slots (num-docs sub-readers) self
    (when (= num-docs -1)
      (setf num-docs (reduce #'+ sub-readers :key #'num-docs)))
    num-docs))

(defmethod get-document ((self multi-reader) n)
  (with-sub-reader (sub-reader start) self n
    (get-document sub-reader start)))

(defmethod deleted-p ((self multi-reader) n)
  (with-sub-reader (sub-reader start) self n
    (deleted-p sub-reader start)))

(defmethod has-deletions-p ((self multi-reader))
  (slot-value self 'has-deletions-p))

(defmethod do-delete ((self multi-reader) n)
  (with-slots (num-docs has-deletions-p) self
    (setf num-docs -1)
    (with-sub-reader (sub-reader start) self n
      (delete-document sub-reader start))
    (setf has-deletions-p T)))

(defmethod do-undelete-all ((self multi-reader))
  (with-slots (num-docs sub-readers has-deletions-p) self
    (setf num-docs -1)
    (dosequence (sub-reader sub-readers)
      (undelete-all sub-reader))
    (setf has-deletions-p NIL)))

(defgeneric reader-index (multi-reader n))

(defmethod reader-index ((self multi-reader) n)
  (with-slots (sub-readers starts) self
    (let ((lo 0)
	  (hi (- (length sub-readers) 1)))
      (while (>= hi lo)
	(let* ((mid (floor (+ lo hi) 2))
	       (mid-value (aref starts mid)))
	  (cond ((< n mid-value)
		 (setf hi (- mid 1)))
		((> n mid-value)
		 (setf lo (+ mid 1)))
		(T
		 (while (and (< (+ mid 1) (length sub-readers))
			     (= (aref starts (+ mid 1)) mid-value))
		   (incf mid))
		 (return-from reader-index mid)))))
      hi)))

(defmethod has-norms-p ((self multi-reader) field)
  (some #'(lambda (sub-reader)
	    (has-norms-p sub-reader field))
	(slot-value self 'sub-readers)))

(defmethod fake-norms ((self multi-reader))
  (with-slots (ones) self
    (setf ones (create-fake-norms (max-doc self)))))

(defmethod get-norms ((self multi-reader) field)
  (with-slots (norms-cache sub-readers starts) self
    (let ((bytes (table-value norms-cache field)))
      (if bytes
	  bytes
	  (if (not (has-norms-p self field))
	      (fake-norms self)
	      (let ((bytes (make-array (max-doc self))))
		(dosequence (sub-reader sub-readers :index i)
		  (get-norms-into sub-reader field bytes (aref starts i)))
		(setf (table-value norms-cache field) bytes)))))))

(defmethod get-norms-into ((self multi-reader) field buf offset)
  (with-slots (norms-cache max-doc sub-readers starts) self
    (let ((bytes (table-value norms-cache field)))
      (when (and (null bytes) (not (has-norms-p self field)))
	(setf bytes (fake-norms self)))
      (if bytes
	  (replace buf
		   bytes
		   :start1 offset :end1 (+ offset max-doc)
		   :start2 0 :end2 max-doc)
	  (dosequence (sub-reader sub-readers :index i)
	    (get-norms-into sub-reader field buf (+ offset (aref starts i))))))))

(defmethod do-set-norm ((self multi-reader) n field value)
  (remtable (slot-value self 'norms-cache) field)
  (with-sub-reader (sub-reader start) self n
    (set-norm sub-reader start field value)))

(defmethod terms ((self multi-reader))
  (with-slots (sub-readers starts) self
    (make-instance 'multi-term-enum
		   :readers sub-readers
		   :starts starts
		   :term nil)))

(defmethod terms-from ((self multi-reader) term)
  (with-slots (sub-readers starts) self
    (make-instance 'multi-term-enum
		   :readers sub-readers
		   :starts starts
		   :term term)))

(defmethod term-doc-freq ((self multi-reader) term)
  (reduce #'+ (slot-value self 'sub-readers)
	  :key #'(lambda (sub-reader) (term-doc-freq sub-reader term))))

(defmethod term-docs ((self multi-reader))
  (with-slots (sub-readers starts) self
    (make-instance 'multi-term-doc-enum
		   :readers sub-readers
		   :starts starts)))

(defmethod term-positions ((self multi-reader))
  (with-slots (sub-readers starts) self
    (make-instance 'multi-term-doc-pos-enum
		   :readers sub-readers
		   :starts starts)))

(defmethod do-commit ((self multi-reader))
  (dosequence (reader (slot-value self 'sub-readers))
    (commit reader)))

(defmethod do-close ((self multi-reader))
  (dosequence (reader (slot-value self 'sub-readers))
    (close reader)))

(defmethod get-field-names ((self multi-reader) &optional (field-option T))
  (remove-duplicates (reduce #'append (slot-value self 'sub-readers)
			     :key #'(lambda (reader) (get-field-names reader field-option)))
		     :test #'equal))


(defclass multi-term-enum (term-enum)
  ((doc-freq :reader doc-freq)
   (term :initform nil :reader term)
   (term-buffer)
   (queue)))

(defmethod initialize-instance :after ((self multi-term-enum) &key readers starts term)
  (with-slots (queue) self
    (setf queue (make-instance 'segment-merge-queue
			       :max-size (length readers)))
    (dosequence (reader readers :index i)
      (let* ((term-enum (if term (terms-from reader term) (terms reader)))
	     (smi (make-instance 'segment-merge-info
				 :base (aref starts i)
				 :term-enum term-enum
				 :reader reader)))
	(if (or (and (null term) (next? smi)) (term term-enum))
	    (queue-push queue smi)
	    (close smi))))
    (when (and term (> (size queue) 0))
      (next? self))))

(defmethod next? ((self multi-term-enum))
  (with-slots (queue term-buffer term doc-freq) self
    (let ((top (queue-top queue)))
    (if (null top)
	(setf term-buffer nil)
	(progn
	  (setf term (term (term-buffer top)))
	  (setf doc-freq 0)
	  (while (and top (term= term (term-buffer top)))
	    (queue-pop queue)
	    (incf doc-freq (doc-freq (term-enum top)))
	    (if (next? top)
		(queue-push queue top)
		(close top))
	    (setf top (queue-top queue)))
	  T)))))

(defmethod close ((self multi-term-enum))
  (with-slots (queue) self
    (close queue)))


(defclass multi-term-doc-enum (term-doc-enum)
  ((readers :initarg :readers :reader readers)
   (starts :initarg :starts :reader starts)
   (term :reader term)
   (base :initform 0 :reader base)
   (pointer :initform 0 :reader pointer)
   (reader-term-docs)
   (current :reader current)))

(defmethod print-object ((self multi-term-doc-enum) stream)
  (print-unreadable-object (self stream :type T)
    (let ((term (term self)))
      (format stream "~S:~S" (term-field term) (term-text term)))))

(defmethod initialize-instance :after ((self multi-term-doc-enum) &key)
  (with-slots (reader-term-docs readers) self
    (setf reader-term-docs (make-array (length readers) :initial-element nil))))

(defmethod doc ((self multi-term-doc-enum))
  (with-slots (base current) self
    (+ base (doc current))))

(defmethod freq ((self multi-term-doc-enum))
  (freq (slot-value self 'current)))

(defmethod seek ((self multi-term-doc-enum) term)
  (with-slots (base pointer current) self
    (setf (slot-value self 'term) term
	  base 0
	  pointer 0
	  current nil)))

(defmethod next? ((self multi-term-doc-enum))
  (with-slots (current pointer readers base starts) self
    (cond ((and current (next? current))
	   T)
	  ((< pointer (length readers))
	   (setf base (aref starts pointer)
		 current (multi-term-docs self pointer)
		 pointer (1+ pointer))
	   (next? self))
	  (T NIL))))


(defmethod read-segment-term-doc-enum ((self multi-term-doc-enum) docs freqs &optional start)
  (declare (ignore start))
  (with-slots (current pointer readers base starts) self
    (let ((got 0)
	  (last-got 0)
	  (needed (length docs)))
      (while T
	(while (null current)
	  (if (< pointer (length readers))
	      (setf base (aref starts pointer)
		    current (multi-term-docs self pointer)
		    pointer (1+ pointer))
	      (return-from read-segment-term-doc-enum got)))
	(setf got (read-segment-term-doc-enum current docs freqs got))
	(if (= got last-got)
	    (setf current nil)
	    (let ((b base))
	      (loop for i from last-got below got
		 do (incf (aref docs i) b))
	      (if (= got needed)
		  (return-from read-segment-term-doc-enum got)
		  (setf last-got got))))))))

(defmethod skip-to ((self multi-term-doc-enum) target)
  (loop
     do (when (not (next? self))
	  (return-from skip-to NIL))
       while (> target (doc self)))
  T)

(defgeneric multi-term-docs (multi-term-doc-enum i))

(defmethod multi-term-docs ((self multi-term-doc-enum) i)
  (with-slots (term reader-term-docs readers) self
    (if (null term)
	nil
	(let ((result (aref reader-term-docs i)))
	  (when (null result)
	    (setf result (setf (aref reader-term-docs i) (term-docs-from-reader self (aref readers i)))))
	  (seek result term)
	  result))))

(defgeneric term-docs-from-reader (multi-term-doc-enum reader))

(defmethod term-docs-from-reader ((self multi-term-doc-enum) reader)
  (term-docs reader))

(defmethod close ((self multi-term-doc-enum))
  (dosequence (rtd (slot-value self 'reader-term-docs))
    (when rtd (close rtd))))


(defclass multi-term-doc-pos-enum (multi-term-doc-enum)
  ())

(defmethod term-docs-from-reader ((self multi-term-doc-pos-enum) reader)
  (term-positions reader))

(defmethod next-position ((self multi-term-doc-pos-enum))
  (next-position (slot-value self 'current)))
