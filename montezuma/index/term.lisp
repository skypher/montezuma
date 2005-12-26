(in-package #:montezuma)

(defstruct (term (:constructor make-term (field text)))
  field
  text)

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
