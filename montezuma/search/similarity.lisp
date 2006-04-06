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

;; Verified (for a couple values anyway) to be consistent with
;; Lucene svn.  I couldn't find enough documentation on the
;; Ferret/Ruby way to replicate it in Lisp.

(defun similarity-byte-to-float (b)
  (assert (and (>= 255 b 0)))
  (byte315-to-float b))

(defun similarity-float-to-byte (f)
  (float-to-byte315 (float f 1.0s0)))


;; Corresponds to org.apache.lucene.util.SmallFloat.byte315ToFloat.

(defun byte315-to-float (b)
  (if (= b 0)
      0.0
      (let ((bits (ash (logand b #xff) (- 24 3))))
	(incf bits (ash (- 63 15) 24))
	(int-bits-to-float bits))))


;; Corresponds to org.apache.lucene.util.SmallFloat.floatToByte315.

(defun float-to-byte315 (f)
  (let* ((bits (float-to-raw-int-bits f))
	 (smallfloat (ash bits (- (- 24 3)))))
    (cond ((< smallfloat (ash (- 63 15) 3))
	   (if (<= bits)
	       0
	       1))
	  ((>= smallfloat (+ (ash (- 63 15) 3) #x100))
	   -1)
	  (T
	   (- smallfloat (ash (- 63 15) 3))))))


;; Uses the algorithm described in
;; <http://java.sun.com/j2se/1.4.2/docs/api/java/lang/Float.html#intBitsToFloat(int)>

(defun int-bits-to-float (bits)
  (let* ((s (if (= (ash bits -31) 0) 1 -1))
	 (e (logand (ash bits -23) #xff))
	 (m (if (= e 0)
		(ash (logand bits #x7fffff) 1)
		(logior (logand bits #x7fffff) #x800000))))
    (float (* s m (expt 2 (- e 150))))))


;; Based on Pascal Bourguignon's description of his gen-ieee-encoding
;; macro (see <http://paste.lisp.org/display/4371>).  The only
;; difference is that his macro generated code that added 151 to the
;; exponent instead of 150, which didn't give results that matched
;; Java's.  I have only a vague idea of why changing it to add 150 to
;; the exponent gives us correct results.

(defun float-to-raw-int-bits (f)
  (multiple-value-bind (mantissa exponent sign)
      (integer-decode-float f)
    (dpb (if (minusp sign) 1 0)
	 (byte 1 31)
	 (dpb (+ 150 exponent) (byte 8 23) (ldb (byte 23 0) mantissa)))))
    

(defparameter *norm-table* 
  (make-array 256
              :initial-contents (loop for i from 0 below 256
                                      collecting (similarity-byte-to-float i))))

(defun similarity-encode-norm (f)
  (similarity-float-to-byte f))

(defun similarity-decode-norm (b)
  (aref *norm-table* (logand b #xff)))

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



