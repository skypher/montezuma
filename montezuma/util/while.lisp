(in-package #:montezuma)

(defmacro while (expr &body body)
  `(do ()
       ((not ,expr))
     ,@body))

#||
(defmacro dosequence ((var sequence &optional ret) &body body)
  `(let ((,var nil))
     (map nil
	  #'(lambda (,var)
	      ,@body body)
	  ,sequence)
     ,ret))
||#

(defmacro dosequence ((var sequence &key result index) &body body)
  `(let ((,var nil)
	 ,@(if index `((,index 0)) nil))
     (map nil
	  #'(lambda (,var)
	      ,@body
	      ,@(if index `((incf ,index)) nil))
	  ,sequence)
     ,result))
