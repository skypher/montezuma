(in-package #:montezuma)

;; Used to support the object cloning protocol.  I don't like that
;; this isn't standard, and I have a hunch that the cloning protocol
;; should go away.  But it is pretty convenient...

(defun class-slots (class)
  #+openmcl (ccl:class-slots class)
  #+sbcl (sb-mop:class-slots class))
  
(defun slot-definition-name (slot-defn)
  #+openmcl (ccl:slot-definition-name slot-defn)
  #+sbcl (sb-mop:slot-definition-name slot-defn))


;; Something approximating the Ruby clone protocol.

(defun clone (object)
  (let ((clone (clone-object object)))
    (initialize-copy clone object)
    clone))

(defmethod clone-object ((object T))
  (let ((copy (allocate-instance (class-of object))))
    (loop for slot in (class-slots (class-of object))
       when (slot-boundp object (slot-definition-name slot))
       do (setf (slot-value copy (slot-definition-name slot))
		(slot-value object (slot-definition-name slot))))
    copy))


(defmethod initialize-copy (self o)
  (declare (ignore self) (ignore o)))

