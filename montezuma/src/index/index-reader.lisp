(in-package #:montezuma)

(defparameter *index-reader-filename-extensions*
  '("cfs"
    "fnm"
    "fdx"
    "fdt"
    "tii"
    "tis"
    "frq"
    "prx"
    "del"
    "tvx"
    "tvd"
    "tvf"
    "tvp"))

(defclass index-reader ()
  ((directory :initarg :directory :reader directory)
   (close-directory-p :initarg :close-directory-p :initform NIL)
   (segment-infos :initarg :segment-infos :initform nil)
   (directory-owner :initarg :directory-owner :initform NIL)
   (has-changes-p :initform NIL)
   (stale :initform NIL)))

(defmethod initialize-instance :after ((self index-reader) &key)
  ;;(care-when-finalized self)
  )

(defun open-index-reader (directory &key (close-directory-p T) (infos nil))
  (if (null directory)
      (setf directory (make-instance 'ram-directory))
      (when (stringp directory)
	(setf directory (make-fs-directory directory :create-p NIL))))
  (when (null infos)
    (setf infos (make-instance 'segment-infos))
    (read-segment-infos infos directory))
  (if (= (size infos) 1)
      (get-segment-reader (segment-info infos 0) :infos infos :close-directory-p close-directory-p)
      (let ((readers (make-array (size infos))))
	(dotimes (i (size infos))
	  (setf (aref readers i) (get-segment-reader (segment-info infos i))))
	(make-instance 'multi-reader
		       :sub-readers readers
		       :directory directory
		       :segment-infos infos
		       :close-directory-p close-directory-p))))

(defgeneric get-current-version (index-reader directory))

(defmethod get-current-version ((self index-reader) directory)
  (segment-infos-read-current-version directory))

(defgeneric get-term-vectors (index-reader doc-number))

(defgeneric get-term-vector (index-reader doc-number field))

(defgeneric index-reader-index-exists (directory))
(defmethod index-reader-index-exists (directory)
  (file-exists-p directory "segments"))

(defgeneric num-docs (index-reader))

(defgeneric max-doc (index-reader))

(defgeneric get-document (index-reader n))

(defgeneric get-document-with-term (index-reader term))
(defmethod get-document-with-term ((self index-reader) term)
  (let ((docs (term-docs-for self term)))
    (if (null docs)
	nil
	(unwind-protect
	     (if (next? docs)
		 (get-document self (doc docs))
		 nil)
	  (close docs)))))

(defgeneric document-deleted-p (index-reader n))

(defgeneric has-deletions-p (index-reader))

(defgeneric has-norms-p (index-reader field))

(defmethod has-norms-p ((self index-reader) field)
  (get-norms self field))

(defgeneric get-norms (index-reader field))

(defgeneric get-norms-info (index-reader field bytes offset))

(defgeneric set-norm (index-reader doc field value))

(defmethod set-norm ((self index-reader) doc field value)
  (when (floatp value)
    (setf value (similarity-encode-norm value)))
  (do-set-norm self doc field value)
  (setf (slot-value self 'has-changes-p) T))

(defgeneric do-set-norm (index-reader doc field value))

(defgeneric terms (index-reader))

(defgeneric term-doc-freq (index-reader term))

(defgeneric term-docs-for (index-reader term))

(defmethod term-docs-for ((self index-reader) term)
  (let ((term-docs (term-docs self)))
    (seek term-docs term)
    term-docs))

(defgeneric term-docs (index-reader))

(defgeneric term-positions-for (index-reader term))

(defmethod term-positions-for ((self index-reader) term)
  (let ((term-positions (term-positions self)))
    (seek term-positions term)
    term-positions))

(defgeneric term-positions (index-reader))

;; FIXME: locks.  ugh.
;;(defmethod acquire-write-lock ...)


(defgeneric latest-p (index-reader))

(defmethod latest-p ((self index-reader))
  (with-slots (directory segment-infos) self
    (eql (segment-infos-read-current-version directory)
	 (version segment-infos))))

(defgeneric delete (index-reader doc-number))

(defmethod delete ((self index-reader) doc-num)
  (do-delete self doc-num)
  (setf (slot-value self 'has-changes-p) T))

(defgeneric delete-docs-with-term (index-reader term))

(defmethod delete-docs-with-term ((self index-reader) term)
  (let ((docs (term-docs-for self term)))
    (if (null docs)
	0
	(let ((n 0))
	  (unwind-protect
	       (while (next? docs)
		 (delete self (doc docs))
		 (incf n))
	    (close docs))
	  n))))

(defgeneric undelete-all (index-reader))

(defmethod undelete-all ((self index-reader))
  (do-undelete-all self)
  (setf (slot-value self 'has-changes-p) T))

(defgeneric commit (index-reader))

(defmethod commit ((self index-reader))
  (with-slots (has-changes-p directory-owner directory segment-infos) self
    (when has-changes-p
      (if directory-owner
	  (progn
	    (do-commit self)
	    (write-segment-infos segment-infos directory))
	  (do-commit self)))
    (setf has-changes-p NIL)))

;;(defmethod when-finalized ((self index-reader))
;;  (close self))

(defmethod close ((self index-reader))
;  (ignore-finalization self)
  (commit self)
  (do-close self)
  (with-slots (directory close-directory-p) self
    (when close-directory-p
      (close directory))))


