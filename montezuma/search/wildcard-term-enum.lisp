(in-package #:montezuma)

(defclass wildcard-term-enum (filtered-term-enum)
  ((end-enum :initform NIL :reader end-enum)
   (search-term :initarg :search-term)
   (field)
   (pre)
   (pre-len)
   (pattern)))


(defparameter +wildcard-string+ #\*)
(defparameter +wildcard-char+ #\?)

(defmethod initialize-instance :after ((self wildcard-term-enum) &key reader)
  (with-slots (field search-term pre pre-len pattern) self
    (setf field (term-field search-term))
    (let* ((text (term-text search-term))
	   (len (length text)))
      (let ((sidx (or (position +wildcard-string+ text) len))
	    (cidx (or (position +wildcard-char+ text) len)))
	(let ((idx (min sidx cidx)))
	  (setf pre (subseq text 0 idx))
	  (setf pre-len idx)
	  ;; FIXME: is this really correct?
	  (setf pattern (cl-ppcre:create-scanner
			 (format nil "^~A$"
				 (cl-ppcre:regex-replace-all
				  "\\\\([?])"
				  (cl-ppcre:regex-replace-all
				   "\\\\([*])"
				   (cl-ppcre:quote-meta-chars (if (< idx len)
								  (subseq text idx len)
								  ""))
				   ".\\1")
				  "."))))
	  (setf (enum self) (terms-from reader
					(make-term (term-field search-term)
						   pre))))))))

(defmethod term-compare ((self wildcard-term-enum) term)
  (with-slots (field pre-len pre pattern enum-enum end-enum) self
    (when (string= field (term-field term))
      (let ((search-text (term-text term)))
	(when (string= pre search-text :start2 0 :end2 pre-len)
	  (return-from term-compare
	    (cl-ppcre:scan pattern search-text :start pre-len :end (length search-text))))))
    (setf end-enum T)
    NIL))

(defmethod difference ((self wildcard-term-enum))
  1.0)

(defmethod close :after ((self wildcard-term-enum))
  (with-slots (pattern field) self
    (setf pattern nil
	  field nil)))

	    