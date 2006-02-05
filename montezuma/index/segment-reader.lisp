(in-package #:montezuma)

(defclass segment-reader (index-reader)
  ((segment)
   (cfs-reader :initform nil)
   (deleted-docs)
   (deleted-docs-dirty-p)
   (field-infos)
   (fields-reader)
   (term-infos)
   (freq-stream)
   (prox-stream)
   (norms :initform (make-hash-table :test #'equal))
   (norms-dirty-p)
   (tv-reader-orig))
)

(defmethod initialize-instance :after ((self index-reader) &key info)
  (with-slots (segment directory deleted-docs field-infos fields-reader
		       term-infos deleted-docs-dirty-p freq-stream
		       prox-stream norms norms-dirty-p) self
    (setf segment (segment-info-name info))
    (let ((dir directory))
      (when (uses-compound-file-p info)
	(setf cfs-reader (make-instance 'compound-file-reader
					:directory directory
					:name (add-file-extension segment "cfs")))
	(setf dir cfs-reader))
      (setf field-infos (make-instance 'field-infos
				       :directory dir
				       :name (add-file-extension segment "fnm")))
      (setf fields-reader (make-instance 'fields-reader
					 :directory dir
					 :segment segment
					 :field-infos field-infos))
      (setf term-infos (make-instance 'term-infos-reader
				      :directory dir
				      :segment segment
				      :field-infos field-infos))
      (setf deleted-docs nil)
      (setf deleted-docs-dirty-p NIL)
      (when (has-deletions-p info)
	(setf deleted-docs (read-bit-vector directory (add-file-extension segment "del"))))
      (setf freq-stream (open-segment-file dir segment "frq" :input))
      (setf prox-stream (open-segment-file dir segment "prx" :input))
      (setf norms-dirty-p NIL)
      (open-norms self dir)
      (setf tv-reader-orig NIL)
      (when (has-vectors-p field-infos)
	(setf tv-reader-orig (make-instance 'term-vectors-reader
					    :directory dir
					    :segment segment
					    :field-infos field-infos))))))

(defmethod do-commit ((self segment-reader))
  (with-slots (deleted-docs-dirty-p deleted-docs norms norms-dirty-p segment-reader
				    undelete-all-p) self
    (when deleted-docs-dirty-p
      (write deleted-docs directory (add-file-extension segment "tmp"))
      (rename-file directory (add-file-extension segment "tmp")
		   (add-file-extension segment "del")))
    (when (and undelete-all-p
	       (file-exists-p directory (add-file-extension segment "del")))
      (delete-file directory (add-file-extension segment "del")))
    (when norms-dirty-p
      (loop for norm being the hash-values in norms
	 do (when (dirty-p norm)
	      (re-write norm directory segment (max-doc self) cfs-reader))))
    (setf deleted-docs-dirty-p NIL
	  norms-dirty-p NIL
	  undelete-all-p NIL)))

(defmethod do-close ((self segment-reader))
  (with-slots (fields-reader term-infos freq-stream prox-stream
			     tv-reader-orig cfs-reader) self
    ;; FIXME some thread specific cache clearing?
    (close fields-reader)
    (close term-infos)
    (when freq-stream (close freq-stream))
    (when prox-stream (close prox-stream))
    (close-norms self)
    (when tv-reader-orig (close tv-reader-orig))
    (when cfs-reader (close cfs-reader))))

(defmethod has-deletions-p ((si segment-info))
  (file-exists-p (directory si) (add-file-extension (segment-info-name si) "del")))

(defmethod uses-compound-file-p ((si segment-info))
  (file-exists-p (directory si) (add-file-extension (segment-info-name si) "cfs")))

(defmethod has-separate-norms-p ((si segment-info))
  (let ((name-pattern (format nil "~A.s" (segment-info-name si))))
    (some #'(lambda (file) (string-begins file name-pattern))
	  (files (directory si)))))

(defmethod do-delete ((self segment-reader) doc-num)
  (with-slots (deleted-docs deleted-docs-dirty-p undelete-all-p) self
    (when (null deleted-docs)
      (setf deleted-docs (make-bit-vector)))
    (setf deleted-docs-dirty-p T)
    (setf undelete-all-p NIL)
    (setf (bit-aref deleted-docs) doc-num)))

(defmethod do-undelete-all ((self segment-reader))
  (with-slots (deleted-docs deleted-docs-dirty-p undelete-all-p) self
    (setf deleted-docs NIL
	  deleted-docs-dirty-p NIL
	  undelete-all-p T)))

(defmethod file-names ((self segment-reader))
  (let ((filenames '())
	(segment (slot-value self 'segment))
	(directory (slot-value self 'directory)))
    (dolist (ext *index-filename-extensions*)
      (let ((name (add-file-extension segment ext)))
	(when (file-exists-p directory name)
	  (push name filenames))))
    ()))

(defmethod terms ((self segment-reader))
  (terms (slot-value self 'term-infos)))

(defmethod terms-from ((self segment-reader) term)
  (terms-from (slot-value self 'term-infos) term))

(defmethod get-document ((self segment-reader) n)
  (when (deleted-p self n)
    (error "Document ~S in ~S has been deleted." n self))
  (doc (slot-value self 'fields-reader) n))

(defmethod deleted-p ((self segment-reader) n)
  (let ((deleted-docs (slot-value self 'deleted-docs)))
    (and deleted-docs (= (bit-aref deleted-docs n) 1))))

(defmethod term-docs ((self segment-reader))
  (make-instance 'segment-term-doc-enum
		 :thing self))

(defmethod term-positions ((self segment-reader))
  (make-instance 'segment-term-doc-pos-enum
		 :thing self))

(defmethod doc-freq ((self segment-reader) term)
  (let ((ti (get-term-info (slot-value self 'term-infos) term)))
    (if ti
	(doc-freq ti)
	0)))

(defmethod num-docs ((self segment-reader))
  (let ((n (max-doc self))
	(deleted-docs (slot-value self 'deleted-docs)))
    (when deleted-docs
      (decf n (bit-vector-count deleted-docs)))
    n))

(defmethod max-doc ((self segment-reader))
  (size (slot-value self 'fields-reader)))

(defmethod get-field-names ((self segment-reader) &optional (field-option T))
  (check-type field-option (member T :unindexed :indexed :indexed-no-term-vector :term-vector
				   :indexed-with-term-vector :term-vector-with-position
				   :term-vector-with-offset :term-vector-with-position-offset))
  (let ((field-set '()))
    (dosequence (field (all-fields (slot-value self 'field-infos)))
      (cond ((eq field-option T)
	     (pushnew (name field) field-set))
	    ((and (not (field-indexed-p field)) (eq field-option :unindexed))
	     (pushnew (name field) field-set))
	    ((and (field-indexed-p field) (eq field-option :indexed))
	     (pushnew (name field) field-set))
	    ((and (field-indexed-p field) (not (field-store-term-vector-p field))
		  (eq field-option :indexed-no-term-vector))
	     (pushnew (name field) field-set))
	    ((and (field-store-term-vector-p field)
		  (not (field-store-positions-p field))
		  (not (field-store-offsets-p field))
		  (eq field-option :term-vector))
	     (pushnew (name field) field-set))
	    ((and (field-indexed-p field) (field-store-term-vector-p field)
		  (eq field-option :indexed-with-term-vector))
	     (pushnew (name field) field-set))
	    ((and (field-store-positions-p field) (not (field-store-offsets-p field))
		  (eq field-option :term-vector-with-position))
	     (pushnew (name field) field-set))
	    ((and (field-store-offsets-p field) (not (field-store-positions-p field))
		  (eq field-option :term-vector-with-offset))
	     (pushnew (name field) field-set))
	    ((and (field-store-offsets-p field) (field-store-positions-p field)
		  (eq field-option :term-vector-with-position-offset))
	     (pushnew (name field) field-set))))
    field-set))

(defmethod has-norms-p ((self segment-reader) field)
  (check-type field string)
  (multiple-value-bind (value has-key-p)
      (gethash field (slot-value self 'norms))
    (declare (ignore value))
    has-key-p))


(defmethod get-norms ((self segment-reader) field)
  (check-type field string)
  (let ((norm (gethash field (slot-value self 'norms))))
    (if (null norm)
	nil
	(progn
	  (when (null (bytes norm))
	    (let ((bytes (* " " (max-doc self))))
	      (get-norms-info self field bytes 0)
	      (setf (bytes norm) bytes)))
	  (bytes norm)))))

(defmethod do-set-norm ((self segment-reader) doc field value)
  (check-type field string)
  (let ((norm (gethash field (slot-value self 'norms))))
    (if (null norm)
	(let ((max-doc (max-doc self)))
	  (replace bytes (fake-norms self)
		   :start1 offset :end1 max-doc
		   :start2 0 :end2 max-doc))
	(if (null (bytes norm))
	    (let ((max-doc (max-doc self)))
	      (replace bytes (bytes norm)
		       :start1 offset :end1 max-doc
		       :start2 0 :end2 max-doc))
	    (let ((norm-stream (clone (is norm))))
	      (unwind-protect
		   (progn
		     (seek norm-stream 0)
		     (read-bytes norm-stream bytes offset (max-doc self)))
		(close norm-stream)))))))

	  