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
	  (setf (aref readers i) (get-segment-reader (elt infos i))))
	(make-instance 'multi-reader
		       :readers readers
		       :directory directory
		       :infos infos
		       :close-directory-p close-directory-p))))

(defmethod get-current-version ((self index-reader) directory)
  (segment-infos-read-current-version directory))

(defmethod get-term-vectors ((self index-reader) doc-number)
  (error "~S is not implemented by ~S." 'get-term-vectors self))

(defmethod get-term-vector ((self index-reader) doc-number field)
  (error "~S is not implemented by ~S." 'get-term-vector self))

(defmethod index-reader-index-exists (directory)
  (file-exists-p directory "segments"))

(defmethod num-docs ((self index-reader))
  (error "~S is not implemented by ~S." 'num-docs self))

(defmethod max-doc ((self index-reader))
  (error "~S is not implemented by ~S." 'max-doc self))

(defmethod get-doc ((self index-reader) n)
  (error "~S is not implemented by ~S." 'get-doc self))

(defmethod get-document-with-term ((self index-reader) term)
  (let ((docs (term-docs-for self term)))
    (if (null docs)
	nil
	(unwind-protect
	     (if (next docs)
		 (get-document self (doc docs))
		 nil)
	  (close docs)))))

(defmethod document-deleted-p ((self index-reader) n)
  (error "~S is not implemented by ~S." 'document-deleted-p self))

(defmethod has-deletions-p ((self index-reader))
  (error "~S is not implemented by ~S." 'has-deletions-p self))

(defmethod has-norms-p ((self index-reader) field)
  (get-norms self field))

(defmethod get-norms ((self index-reader) field)
  (error "~S is not implemented by ~S." 'get-norms self))

(defmethod get-norms-info ((self index-reader) field bytes offset)
  (error "~S is not implemented by ~S." 'get-norms-info self))

(defmethod set-norm ((self index-reader) doc field value)
  (when (floatp value)
    (setf value (similarity-encode-norm value)))
  (do-set-norm self doc field value)
  (setf (slot-value self 'has-changes-p) T))

(defmethod do-set-norm ((self index-reader) doc field value)
  (error "~S is not implemented by ~S." 'do-set-norm self))

(defmethod terms ((self index-reader))
  (error "~S is not implemented by ~S." 'terms self))

(defmethod terms-from ((self index-reader) tee)
  (error "~S is not implemented by ~S." 'terms-from self))

(defmethod term-doc-freq ((self index-reader) term)
  (error "~S is not implemented by ~S." 'doc-freq self))

(defmethod term-docs-for ((self index-reader) term)
  (let ((term-docs (term-docs self)))
    (seek term-docs term)
    term-docs))

(defmethod term-docs ((self index-reader))
  (error "~S is not implemented by ~S." 'doc-freq self))

(defmethod term-positions-for ((self index-reader) term)
  (let ((term-positions (term-positions self)))
    (seek term-positions term)
    term-positions))

(defmethod term-positions ((self index-reader))
  (error "~S is not implemented by ~S." 'term-positions self))

;; FIXME: locks.  ugh.
;;(defmethod acquire-write-lock ...)


(defmethod latest-p ((self index-reader))
  (with-slots (directory segment-infos) self
    (eql (segment-infos-read-current-version directory)
	 (version segment-infos))))

(defmethod delete ((self index-reader) doc-num)
  (do-delete self doc-num)
  (setf (slot-value self 'has-changes-p) T))

(defmethod delete-docs-with-term ((self index-reader) term)
  (let ((docs (term-docs-for self term)))
    (if (null docs)
	0
	(let ((n 0))
	  (unwind-protect
	       (while (next docs)
		 (delete self (doc docs))
		 (incf n))
	    (close docs))
	  n))))

(defmethod undelete-all ((self index-reader))
  (do-undelete-all self)
  (setf (slot-value self 'has-changes-p) T))

(defmethod commit ((self index-reader))
  (with-slots (has-changes-p directory-owner directory segment-infos) self
    (when has-changes-p
      (if directory-owner
	  (progn
	    (do-commit self)
	    (write segment-infos directory))
	  (do-commit self)))
    (setf has-changes-p NIL)))

(defmethod close ((self index-reader))
  (commit self)
  (do-close self)
  (with-slots (directory close-directory-p) self
    (when close-directory-p
      (close directory))))
