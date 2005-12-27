(in-package #:montezuma)

(defclass term-enum ()
  ())

(defgeneric next (enum))
(defgeneric term (enum))
(defgeneric doc-freq (enum))
(defgeneric close (enum))

(defmethod skip-to ((self term-enum) target)
  (while (term> target (term self))
    (unless (next self)
      (return-from skip-to NIL)))
  T)

  