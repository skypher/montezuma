(in-package #:montezuma)

(defclass term-enum ()
  ())

(defmethod skip-to ((self term-enum) target)
  (while (term> target (term self))
    (unless (next self)
      (return-from skip-to NIL)))
  T)

  