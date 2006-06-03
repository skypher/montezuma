(in-package #:montezuma)

;; Used to support the object cloning protocol.  I don't like that
;; this isn't standard, and I have a hunch that the cloning protocol
;; should go away.  But it is pretty convenient...

(defun class-slots (class)
  #+(or digitool openmcl) (ccl:class-slots class)
  #+sbcl (sb-mop:class-slots class))
  
(defun slot-definition-name (slot-defn)
  #+(or digitool openmcl) (ccl:slot-definition-name slot-defn)
  #+sbcl (sb-mop:slot-definition-name slot-defn))


;; Something approximating the Ruby clone protocol.

(defun clone (object)
  (let ((clone (clone-object object)))
    (initialize-copy clone object)
    clone))

(defmethod clone-object ((object T))
  (let ((copy (allocate-instance (class-of object))))
    (loop for slot in (class-slots (class-of object))
	  do (let ((name (slot-definition-name slot)))
	       (when (slot-boundp object name)
		 (setf (slot-value copy name)
		       (slot-value object name)))))
    copy))

(defmethod initialize-copy (self o)
  (declare (ignore self) (ignore o)))

(defmethod clone-object ((array array))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (let ((dimensions (array-dimensions array))
          (element-type (array-element-type array))
          (adjustable (adjustable-array-p array))
          (fill-pointer (when (array-has-fill-pointer-p array)
                          (fill-pointer array))))
      (let ((new-array
             (apply #'make-array
                    (list* dimensions
                           :element-type element-type
                           :adjustable adjustable
                           :fill-pointer fill-pointer
                           :displaced-to displaced-to
                           (if displaced-to
                               (list :displaced-index-offset
                                     displaced-index-offset)
                               nil)))))
        (unless displaced-to
          (dotimes (i (array-total-size array))
            (setf (row-major-aref new-array i)
                  (row-major-aref array i))))
        new-array))))
