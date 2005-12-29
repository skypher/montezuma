(in-package #:montezuma)

(defmacro while (expr &body body)
  `(do ()
       ((not ,expr))
     ,@body))

