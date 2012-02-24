(in-package :lamsight2)

(lamsight2-proclamations)

(defparameter *lazy-fulltext-indexing* nil
  "Inhibit indexing on object creation and do it explicitly
   via index-object")

;;
;; Support automatic indexing of model classes
;;

(defpclass fulltext-mixin ()
  ((fulltext :accessor fulltext-fields :initarg :fulltext-fields)))

(defmethod initialize-instance :before ((inst fulltext-mixin) &key fulltext-fields &allow-other-keys)
  (unless fulltext-fields
    (error "Classes inheriting fulltext-mixin must initialize :fulltext-fields")))

(defmethod initialize-instance :after ((inst fulltext-mixin) &rest args)
  (declare (ignore args))
  (unless *lazy-fulltext-indexing*
    (fulltext-lock (index-object inst))))

(defmethod fulltext-document ((inst fulltext-mixin))
  (unless (fulltext-fields inst)
    (error "Field list not defined for class ~A inheriting from fulltext-mixin!"
	   (class-of inst)))
  (auto-create-fulltext-document inst #'object-id 
				 (mapcar #'mklist (fulltext-fields inst))))
				 
  
(defmethod drop-instance :before ((inst fulltext-mixin))
  (ignore-errors 
    (fulltext-lock (unindex-object inst))))

(defmethod (setf closer-mop:slot-value-using-class) 
    (value (obj fulltext-mixin) instance slot-def)
  "Make sure if we change the object value, we update the index!"
  (log-message :fulltext :debug "Updating fulltext index for: ~A" instance)
  (let ((slotname (closer-mop:slot-definition-name slot-def)))
    (when (member slotname (fulltext-fields obj))
      (montezuma:update *fulltext-index* (object-id obj) 
			`((,(symbol-name slotname) . value))))))
  

  
