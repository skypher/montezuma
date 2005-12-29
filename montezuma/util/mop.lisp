(in-package #:montezuma)

(defun class-slots (class)
  #+openmcl (ccl:class-slots class)
  #+sbcl (sb-mop:class-slots class))
  

(defun slot-definition-name (slot-defn)
  #+openmcl (ccl:slot-definition-name slot-defn)
  #+sbcl (sb-mop:slot-definition-name slot-defn))

