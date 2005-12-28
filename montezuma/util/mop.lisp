(in-package #:montezuma)

(defun class-slots (class)
  (ccl:class-slots class))

(defun slot-definition-name (slot-defn)
  (ccl:slot-definition-name slot-defn))
