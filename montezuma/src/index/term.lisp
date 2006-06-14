(in-package #:montezuma)

(defgeneric term-field (term))
  
(defstruct (term (:constructor make-term (field text)) (:conc-name %term-))
  field
  text)

(defmethod term-text ((self term))
  (%term-text self))

(defmethod (setf term-text) (text (self term))
  (setf (%term-text self) text))

(defmethod term-field ((self term))
  (%term-field self))

(defmethod (setf term-field) (field (self term))
  (setf (%term-field self) field))

(defgeneric set-term (term field text))

(defmethod set-term ((self term) field text)
  (setf (%term-text self) text)
  (setf (%term-field self) field))

(defgeneric term-compare (term1 term2))
(defgeneric term= (term1 term2))

(defmethod term-compare ((t1 term) (t2 term))
  (let ((f1 (term-field t1))
	(f2 (term-field t2)))
    (if (string= f1 f2)
	(string-compare (term-text t1) (term-text t2))
	(string-compare f1 f2))))

(defmethod term= ((t1 term) (t2 term))
  (and (string= (term-field t1) (term-field t2))
       (string= (term-text t1) (term-text t2))))

(defun term< (t1 t2)
  (let ((f1 (term-field t1))
	(f2 (term-field t2)))
    (if (string= f1 f2)
	(string< (term-text t1) (term-text t2))
	(string< f1 f2))))

(defun term> (t1 t2)
  (let ((f1 (term-field t1))
	(f2 (term-field t2)))
    (if (string= f1 f2)
	(string> (term-text t1) (term-text t2))
	(string> f1 f2))))

(defgeneric to-string (term))

(defmethod to-string ((self term))
  (format nil "~A:~A" (term-field self) (term-text self)))
