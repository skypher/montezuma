(in-package #:montezuma)

(defstruct (token
	     (:constructor make-token (image start end &key (increment 1) (type :word))))
  image
  start
  end
  (increment 1)
  (type :word))

(defun token-compare (t1 t2)
  (let ((r (- (token-start t2) (token-start t1))))
    (if (not (= r 0))
	r
	(let ((r (- (token-end t2) (token-end t1))))
	  (if (not (= r 0))
	      r
	      (string-compare (token-image t1) (token-image t2)))))))

(defun token= (t1 t2)
  (= 0 (token-compare t1 t2)))

(defmethod term-text ((token token))
  (token-image token))