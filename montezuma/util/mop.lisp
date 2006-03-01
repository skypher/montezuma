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

