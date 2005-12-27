(in-package #:montezuma)


(defparameter +not-a-field+ #xffffffff)  ; -1 in java int

(defparameter +is-indexed+ 0)
(defparameter +store-term-vector+ 1)
(defparameter +store-position+ 2)
(defparameter +store-offset+ 3)
(defparameter +omit-norms+ 4)


(defclass field-infos ()
  ((fi-array :initform (make-array (list 0) :fill-pointer 0 :adjustable T))
   (fi-hash :initform (make-hash-table :test #'equal))))

(defmethod initialize-instance :after ((self field-infos) &key dir name)
  (when (and dir (file-exists-p dir name))
    (let ((input (open-input dir name)))
      (unwind-protect
	   (read self input)
	(close input)))))


;; Automatically adds all of the fields from the document if they
;; haven't been added already. Or it will update the values.
	  
(defmethod add-doc-fields ((self field-infos) doc)
  (dolist (field (fields doc))
    (add-field-info self
		    (field-name field)
		    (field-indexed-p field)
		    (field-store-term-vector-p field)
		    (field-store-positions-p field)
		    (field-store-offsets-p field)
		    (field-omit-norms-p field))))

(defmethod add-fields ((self field-infos) names &key (indexed-p T) store-term-vector store-position
		       store-offset omit-norms)
  (dolist (name names)
    (add-field-info self name indexed-p store-term-vector store-position store-offset omit-norms)))

(defmethod fields ((self field-infos))
  (coerce 'list (slot-value self 'fi-array)))


;; FIXME: there seem to be a lot of bugs in the Ferret-equivalent
;; method:
;; <http://ferret.davebalmain.com/trac/browser/trunk/lib/ferret/index/field_infos.rb#L67>
;;
;; Or maybe I'm misunderstanding.

(defmethod add-field-info ((self field-infos) name &key (indexed-p T) store-term-vector store-position
			   store-offset omit-norms)
  (with-slots (fi-hash) self
    (let ((fi (gethash name fi-hash)))
      (unless fi
	(setf fi (add-field-internal self name indexed-p store-term-vector store-position store-offset omit-norms)))
      (when (and indexed-p (not (field-indexed-p fi)))
	(setf (field-indexed-p fi) T))
      (when (and store-term-vector (not (field-store-term-vector-p fi)))
	(setf (field-store-term-vector-p fi) T))
      (when (and store-position (not (field-store-positions-p fi)))
	(setf (field-store-positions-p fi) T))
      (when (and store-offset (not (field-store-offsets-p fi)))
	(setf (field-store-offsets-p fi) T))
      (when (and (not omit-norms) (field-omit-norms-p fi))
	(setf (field-omit-norms-p fi) NIL))
      fi)))

(defmethod get-field-number ((self field-infos) name)
  (with-slots (fi-hash) self
    (let ((fi (gethash name fi-hash)))
      (if fi
	  (field-number fi)
	  +not-a-field+))))


(defmethod get-field ((self field-infos) index)
  (if (integerp index)
      (if (or (eql index +not-a-field+) (< index 0))
	  (make-instance 'field-info (error "FOO"))
	  (aref (slot-value self 'fi-array) index))
      (gethash index (slot-value self 'fi-hash))))

(defmethod name ((self field-infos) index)
  (if (or (eql index +not-a-field+) (< index 0))
      (make-instance 'field-info (error "FOO"))
      (field-name (get-field self index))))

(defmethod size ((self field-infos))
  (length (slot-value self 'fi-array)))

(defmethod has-vectors-p ((self field-infos))
  (some #'field-store-term-vector-p (slot-value self 'fi-array)))

(defmethod write-to-dir ((self field-infos) dir name)
  (let ((output (create-output dir name)))
    (unwind-protect
	 (write self output)
      (close output))))

(defmethod write ((self field-infos) output)
  (write-vint output (size self))
  (dotimes (i (size self))
    (let ((fi (get-field self i)))
      (write-string output (field-name fi))
      (write-byte output (get-field-info-byte fi)))))

(defmethod read ((self field-infos) input)
  (let ((size (read-vint input)))
    (dotimes (i size)
      (let* ((name (read-string input))
	     (bits (read-byte input))
	     (indexed (logbitp +is-indexed+ bits))
	     (store-term-vector (logbitp +store-term-vector+ bits))
	     (store-position (logbitp +store-position+ bits))
	     (store-offset (logbitp +store-offset+ bits))
	     (omit-norms (logbitp +omit-norms+ bits)))
	(add-field-internal self name indexed store-term-vector store-position
			    store-offset omit-norms)))))


(defmethod add-field-internal ((self field-infos) name indexed store-term-vector store-position store-offset omit-norms)
  (let ((fi (make-instance 'field-info
			   :name name
			   :indexed-p indexed
			   :number (size self)
			   :store-term-vector-p store-term-vector
			   :store-positions-p store-position
			   :store-offsets-p store-offset
			   :omit-norms-p omit-norms)))
    (with-slots (fi-hash fi-array) self
      (vector-push-extend fi fi-array)
      (setf (gethash name fi-hash) fi))))


(defclass field-info ()
  ((name :initarg :name :accessor field-name)
   (number :initarg :number :accessor field-number)

   (indexed-p :initarg :indexed-p :accessor field-indexed-p)
   (store-term-vector-p :initarg :store-term-vector-p :accessor field-store-term-vector-p)
   (store-offsets-p :initarg :store-offsets-p :accessor field-store-offsets-p)
   (store-positions-p :initarg :store-positions-p :accessor field-store-positions-p)
   (omit-norms-p :initarg :omit-norms-p :accessor field-omit-norms-p))
  (:default-initargs
   :store-positions-p NIL
    :store-offsets-p NIL
    :omit-norms-p NIL))

(defmethod get-field-info-byte ((self field-info))
  (let ((bits #x0))
    (flet ((turn-on (index)
	     (setf bits (logior bits (expt 2 index)))))
      (when (field-indexed-p self)
	(turn-on +is-indexed+))
      (when (field-store-term-vector-p self)
	(turn-on +store-term-vector+))
      (when (field-store-positions-p self)
	(turn-on +store-position+))
      (when (field-store-offsets-p self)
	(turn-on +store-offset+))
      (when (field-omit-norms-p self)
	(turn-on +omit-norms+)))
    bits))

