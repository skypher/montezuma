(in-package #:montezuma)

(defclass document ()
  ((boost :initform 1.0 :initarg :boost :accessor boost)
   (fields :initform (make-table :test #'equal))))

(defmethod print-object ((self document) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (fields) self
      (let ((field-names (table-keys fields)))
	(format stream "~{~A~^ ~}" (reverse field-names))))))


(defun make-document ()
  (make-instance 'document))

(defmethod all-fields ((self document))
  (with-slots (fields) self
    (reduce #'append (table-values fields))))

(defmethod field-count ((self document))
  (with-slots (fields) self
    (length (table-entries fields))))

(defmethod entry-count ((self document))
  (length (all-fields self)))

(defmethod add-field ((self document) (field field))
  (with-slots (fields) self
    (let* ((name (field-name field))
	   (fields-with-name (table-value fields name)))
      (setf (table-value fields name) (append fields-with-name (list field)))))
  self)

(defmethod remove-field ((self document) name)
  (with-slots (fields) self
    (let ((fields-with-name (table-value fields name))
	  (removed-field nil))
      (when fields-with-name
	(setf removed-field (car fields-with-name))
	(setf (table-value fields name) (cdr fields-with-name)))
      removed-field)))

(defmethod remove-fields ((self document) name)
  (with-slots (fields) self
    (remtable fields name))
  (values))

;; FIXME: I don't like this name.
(defmethod document-field ((self document) name)
  (car (document-fields self name)))

(defmethod document-fields ((self document) name)
  (with-slots (fields) self
    (table-value fields name)))

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
		   
	     

