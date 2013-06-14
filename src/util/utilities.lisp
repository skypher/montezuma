(in-package #:montezuma)

(declaim (inline length-1-list-p))
(defun length-1-list-p (x) 
  "Is x a list of length 1? Note that this is better than the naive \(= \(length x\) 1\) because we don't need to traverse the entire list..."
  (and (consp x) (null (cdr x))))

;;; ---------------------------------------------------------------------------
;;; each method
;;; ---------------------------------------------------------------------------

(defgeneric each (sequence function))

(defmethod each ((self list) block)
  (mapc block self))


(defun parse-float (string)
  (with-standard-io-syntax
    (read-from-string string)))

(defmacro ignore-slot-unbound (&body body)
  "Returns nil instead of throwing slot unboundedness"
  `(handler-case (progn ,@body) (unbound-slot ())))

(defmacro with-slots-ignoring-unbound ( names obj &body body)
  (let ((s-obj (gensym "OBJ")))
    (flet ((make-binding (name)
             `(,name (ignore-slot-unbound (slot-value ,s-obj ',name)))))
      `(let ((,s-obj ,obj))
        (symbol-macrolet
            ,(mapcar #'make-binding names)
          ,@body)))))
