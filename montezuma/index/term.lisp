(in-package #:montezuma)

(defgeneric term-field (term))
(defgeneric term-text (term))

  
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

(defmethod set-term ((self term) field text)
  (setf (%term-text self) text)
  (setf (%term-field self) field))

(defun term-compare (t1 t2)
  (let ((fc (string-compare (term-field t1) (term-field t2))))
    (if (= fc 0)
	(string-compare (term-text t1) (term-text t2))
	fc)))

(defun term= (t1 &rest more)
  (if (null more)
      T
      (do ((terms more (cdr terms)))
	  ((endp terms) T)
	(when (not (= (term-compare t1 (car terms)) 0))
	  (return NIL)))))

(defun term< (t1 &rest more)
  (if (null more)
      T
      (do ((terms more (cdr terms))
	   (previous-term t1 (car terms)))
	  ((endp terms) T)
	(when (not (< (term-compare previous-term (car terms)) 0))
	  (return NIL)))))

(defun term> (t1 &rest more)
  (if (null more)
      T
      (do ((terms more (cdr terms))
	   (previous-term t1 (car terms)))
	  ((endp terms) T)
	(when (not (> (term-compare previous-term (car terms)) 0))
	  (return NIL)))))


(defmethod to-string ((self term))
  (format nil "~A:~A" (term-field self) (term-text self)))
