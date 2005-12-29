(in-package #:montezuma)

(defclass document ()
  ((boost :initform 1.0 :initarg :boost :accessor document-boost)
   (fields :initform (make-hash-table :test #'equal))))

(defmethod print-object ((self document) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (let ((field-names '()))
      (with-slots (fields) self
	(maphash #'(lambda (name fields)
		     (when fields
		       (push name field-names)))
		 fields))
      (format stream "~{~A~^ ~}" (reverse field-names)))))


(defun make-document ()
  (make-instance 'document))

(defmethod all-fields ((self document))
  (with-slots (fields) self
    (let ((all-fields '()))
      (maphash #'(lambda (name fields)
		   (declare (ignore name))
		   (when fields
		     (setf all-fields (append fields all-fields))))
	       fields)
      all-fields)))

(defmethod field-count ((self document))
  (with-slots (fields) self
    (hash-table-count fields)))

(defmethod entry-count ((self document))
  (length (all-fields self)))

(defmethod add-field ((self document) (field field))
  (with-slots (fields) self
    (let* ((name (field-name field))
	   (fields-with-name (gethash name fields '())))
      (setf (gethash name fields) (append fields-with-name (list field)))))
  self)

(defmethod remove-field ((self document) name)
  (with-slots (fields) self
    (let ((fields-with-name (gethash name fields '()))
	  (removed-field nil))
      (when fields-with-name
	(setf removed-field (car fields-with-name))
	(setf (gethash name fields) (cdr fields-with-name)))
      removed-field)))

(defmethod remove-fields ((self document) name)
  (with-slots (fields) self
    (remhash name fields))
  (values))

(defmethod document-field ((self document) name)
  (car (document-fields self name)))

(defmethod document-fields ((self document) name)
  (with-slots (fields) self
    (gethash name fields)))

(defmethod document-binaries ((self document) name)
  (reduce #'(lambda (a1 &optional a2)
	      (if (null a2)
		  a1
		  (concatenate 'vector a1 a2)))
	  (mapcar #'field-data 
		  (remove-if-not #'field-binary-p (document-fields self name)))))

(defmethod document-values ((self document) name)
  (format nil "~{~A~^ ~}"
	  (mapcar #'field-data
		  (remove-if #'field-binary-p (document-fields self name)))))
		   
	     

