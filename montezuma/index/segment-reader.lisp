(in-package #:montezuma)

(defclass segment-reader (index-reader)
  ((segment)
   (cfs-reader :initform nil)
   (deleted-docs :reader deleted-docs)
   (deleted-docs-dirty-p)
   (field-infos)
   (fields-reader)
   (term-infos :reader term-infos)
   (freq-stream :reader freq-stream)
   (prox-stream :reader prox-stream)
   (norms :initform (make-hash-table :test #'equal))
   (norms-dirty-p)
   (tv-reader-orig :initform nil))
)

(defmethod initialize-instance :after ((self index-reader) &key info)
  (with-slots (segment directory deleted-docs field-infos fields-reader
		       cfs-reader term-infos deleted-docs-dirty-p freq-stream
		       tv-reader-orig prox-stream norms norms-dirty-p) self
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
      (when (has-vectors-p field-infos)
	(setf tv-reader-orig (make-instance 'term-vectors-reader
					    :directory dir
					    :segment segment
					    :field-infos field-infos))))))

(defmethod do-commit ((self segment-reader))
  (with-slots (segment deleted-docs-dirty-p deleted-docs norms norms-dirty-p segment-reader
		       undelete-all-p directory cfs-reader) self
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

(defmethod has-deletions-p ((self segment-reader))
  (slot-value self 'deleted-docs))

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
  (get-doc (slot-value self 'fields-reader) n))

(defmethod deleted-p ((self segment-reader) n)
  (let ((deleted-docs (slot-value self 'deleted-docs)))
    (and deleted-docs (= (bit-aref deleted-docs n) 1))))

(defmethod term-docs ((self segment-reader))
  (make-instance 'segment-term-doc-enum
		 :thing self))

(defmethod term-positions ((self segment-reader))
  (make-instance 'segment-term-doc-pos-enum
		 :parent self))

(defmethod segment-doc-freq ((self segment-reader) term)
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
    (dosequence (field (fields (slot-value self 'field-infos)))
      (cond ((eq field-option T)
	     (pushnew (field-name field) field-set))
	    ((and (not (field-indexed-p field)) (eq field-option :unindexed))
	     (pushnew (field-name field) field-set))
	    ((and (field-indexed-p field) (eq field-option :indexed))
	     (pushnew (field-name field) field-set))
	    ((and (field-indexed-p field) (not (field-store-term-vector-p field))
		  (eq field-option :indexed-no-term-vector))
	     (pushnew (field-name field) field-set))
	    ((and (field-store-term-vector-p field)
		  (not (field-store-positions-p field))
		  (not (field-store-offsets-p field))
		  (eq field-option :term-vector))
	     (pushnew (field-name field) field-set))
	    ((and (field-indexed-p field) (field-store-term-vector-p field)
		  (eq field-option :indexed-with-term-vector))
	     (pushnew (field-name field) field-set))
	    ((and (field-store-positions-p field) (not (field-store-offsets-p field))
		  (eq field-option :term-vector-with-position))
	     (pushnew (field-name field) field-set))
	    ((and (field-store-offsets-p field) (not (field-store-positions-p field))
		  (eq field-option :term-vector-with-offset))
	     (pushnew (field-name field) field-set))
	    ((and (field-store-offsets-p field) (field-store-positions-p field)
		  (eq field-option :term-vector-with-position-offset))
	     (pushnew (field-name field) field-set))))
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
	    (let ((bytes (make-array (max-doc self) :initial-element #\space)))
	      (get-norms-into self field bytes 0)
	      (setf (bytes norm) bytes)))
	  (bytes norm)))))

(defmethod do-set-norm ((self segment-reader) doc field value)
  (check-type field string)
  (let ((norm (gethash field (slot-value self 'norms))))
    (when norm
      (setf (dirty-p norm) T)
      (setf (slot-value self 'norms-dirty-p) T)
      (setf (aref (get-norms self field) doc) value))))

(defmethod get-norms-into ((self segment-reader) field bytes offset)
  (check-type field string)
  (let ((norm (gethash field (slot-value self 'norms)))
	(max-doc (max-doc self)))
    (if (null norm)
	(replace bytes (fake-norms self)
		 :start1 offset :end1 max-doc
		 :start2 0 :end2 max-doc)
	(if (null (bytes norm))
	    (replace bytes (bytes norm)
		     :start1 offset :end1 max-doc
		     :start2 0 :end2 max-doc)
	    (with-open-stream (norm-stream (clone (input-stream norm)))
	      (seek norm-stream 0)
	      (read-bytes norm-stream bytes offset max-doc))))))

(defmethod open-norms ((self segment-reader) cfs-dir)
  (dosequence (fi (fields (slot-value self 'field-infos)))
    (when (and (field-indexed-p fi) (not (field-omit-norms-p fi)))
      (let ((segment (slot-value self 'segment))
	    (directory (slot-value self 'directory)))
	(let ((file-name (add-file-extension segment (format nil "s~S" (field-number fi)))))
	  (when (not (file-exists-p directory file-name))
	    (setf file-name (add-file-extension segment (format nil "f~S" (field-number fi))))
	    (setf directory cfs-dir))
	  (setf (gethash (field-name fi) (slot-value self 'norms))
		(make-instance 'norm
			       :input-stream (open-input directory file-name)
			       :number (field-number fi))))))))

(defmethod close-norms ((self segment-reader))
  (loop for norm being the hash-values in (slot-value self 'norms)
       do (close (input-stream norm))))

(defmethod get-term-vectors-reader ((self segment-reader))
  (with-slots (cached-tv-reader tv-reader-orig) self
    (when (null cached-tv-reader)
      (setf cached-tv-reader (clone tv-reader-orig)))
    cached-tv-reader))

(defmethod get-term-vector ((self segment-reader) doc-number field)
  (with-slots (field-infos tv-reader-orig) self
    (let ((fi (gethash field field-infos)))
      (if (or (null fi) (not (field-store-term-vector-p fi)) (null tv-reader-orig))
	  nil
	  (let ((term-vectors-reader (get-term-vectors-reader self)))
	    (if (null term-vectors-reader)
		nil
		(get-field-tv term-vectors-reader doc-number field)))))))

(defmethod get-term-vectors ((self segment-reader) doc-number)
  (with-slots (tv-reader-orig) self
    (if (null tv-reader-orig)
	nil
	(let ((term-vectors-reader (get-term-vectors-reader self)))
	  (if (null term-vectors-reader)
	      nil
	      (get-tv term-vectors-reader doc-number))))))

(defclass norm ()
  ((input-stream :initarg :input-stream :reader input-stream)
   (dirty-p :initform NIL :reader dirty-p)
   (bytes :accessor bytes)
   (number :initarg :number)))

(defmethod re-write ((self norm) directory segment count cfs-reader)
  (let ((out (open-segment-file directory segment "tmp" :output)))
    (unwind-protect
	 (write-bytes out (bytes self) count)
      (close out))
    (with-slots (number) self
      (let ((filename (if (null cfs-reader)
			  (add-file-extension segment (format nil "f~S" number))
			  (add-file-extension segment (format nil "s~S" number)))))
	(rename-file directory (add-file-extension segment "tmp") filename)
	(setf (slot-value self 'dirty-p) NIL)))))
