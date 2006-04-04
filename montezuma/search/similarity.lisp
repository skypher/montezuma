(in-package #:montezuma)


#|?? Gary King 2006-04-04: why is this machinery necessary?
;;?? what is pack / unpack
(defun similarity-byte-to-float (b)
  (if (= b 0)
    0.0
    (let ((mantissa (logand b #x07))
          (exponent (logand (ash b 3) #x1F)))
      (aref (unpack (pack '(0 0 (ash mantissa -5) (+ exponent 48)) "cccc") "e") 0))))
	
(defun similarity-float-to-byte (f)
  ;; FIXME
  1)
|#

(defun similarity-byte-to-float (b)
  b)

(defun similarity-float-to-byte (f)
  f)

(defparameter *norm-table* 
  (make-array 256
              :initial-contents (loop for i from 0 below 256
                                      collecting (similarity-byte-to-float i))))

(defun similarity-encode-norm (b)
  (aref *norm-table* (logand b #xff)))

(defun similarity-decode-norm (f)
  (similarity-float-to-byte f))

(defclass similarity ()
  ((default-similarity-class :allocation :class :initform 'default-similarity
     :accessor default-similarity-class)))

(defun make-default-similarity ()
  (make-instance
    (default-similarity-class (allocate-instance (find-class 'similarity)))))

(defmethod idf-term ((self similarity) term searcher)
  (idf self (term-doc-freq searcher term) (max-doc searcher)))

(defmethod idf-phrase ((self similarity) terms searcher)
  (let ((idf 0.0))
    (dosequence (term terms)
      (incf idf (idf-term self term searcher))) 
    (values idf)))

;;; ---------------------------------------------------------------------------
;;; default-similarity
;;; ---------------------------------------------------------------------------

(defclass default-similarity (similarity)
  ())

(defmethod length-norm ((self default-similarity) field num-terms)
  (declare (ignore field))
  (/ 1.0 (sqrt num-terms)))

(defmethod query-norm ((self default-similarity) sum-of-squared-weights)
  (/ 1.0 (sqrt sum-of-squared-weights)))

(defmethod tf ((self default-similarity) freq)
  (float (sqrt freq)))

(defmethod sloppy-freq ((self default-similarity) distance)
  (/ 1.0 (1+ distance)))

;; what is the base for log in ruby: e (just like CL)
(defmethod idf ((self default-similarity) doc-freq num-docs)
  (if (zerop num-docs)
    (values 0.0)
    (1+ (log (float (/ num-docs (1+ doc-freq)))))))

(defmethod coord ((self default-similarity) overlap max-overlap)
  (float (/ overlap max-overlap)))



