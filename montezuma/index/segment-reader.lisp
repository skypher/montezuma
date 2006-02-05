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
   (norms)
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
					 :name egment
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
      (setf norms '())
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
      (dosequence (norm norms)
	(when (dirty-p norm)
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
  (file-exists-p (directory si) (add-file-extension (name si) "del")))

(defmethod uses-compound-file-p ((si segment-info))
  (file-exists-p (directory si) (add-file-extension (name si) "cfs")))

(defmethod has-separate-norms-p ((si segment-info))
  (let ((name-pattern (format nil "~A.s" (name si))))
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
