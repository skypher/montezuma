(in-package #:montezuma)

(defclass analyzer ()
  ())

(defgeneric token-stream (tokenizer field string))

(defmethod token-stream ((self analyzer) field string)
  (declare (ignore field))
  (make-instance 'lowercase-tokenizer :input string))

(defgeneric position-increment-gap (analyzer field-name))

(defmethod position-increment-gap ((self analyzer) field-name)
  (declare (ignore field-name))
  0)

(defmethod all-tokens ((self analyzer) field string)
  (let ((token-stream (token-stream self field string)))
    (do ((token (next-token token-stream) (next-token token-stream))
	 (tokens '() (cons token tokens)))
	((null token) (reverse tokens)))))


(defclass whitespace-analyzer (analyzer)
  ())

(defmethod token-stream ((self whitespace-analyzer) field string)
  (declare (ignore field))
  (make-instance 'whitespace-tokenizer :input string))


(defparameter *english-stop-words*
  '("a" "an" "and" "are" "as" "at" "be" "but" "by" "for" "if"
    "in" "into" "is" "it" "no" "not" "of" "on" "or" "s" "such"
    "t" "that" "the" "their" "then" "there" "these"
    "they" "this" "to" "was" "will" "with"))

    
(defclass stop-analyzer (analyzer)
  ((stop-words :initarg :stop-words))
  (:default-initargs 
   :stop-words *english-stop-words*))

(defmethod token-stream ((self stop-analyzer) field string)
  (declare (ignore field))
  (with-slots (stop-words) self
    (make-instance 'stop-filter
		   :input (make-instance 'lowercase-tokenizer :input string)
		   :stop-set stop-words)))


(defclass standard-analyzer (stop-analyzer)
  ())

(defmethod token-stream ((self standard-analyzer) field string)
  (declare (ignore field))
  (make-instance 'stop-filter
		 :input (make-instance 'lowercase-filter
				       :input (make-instance 'standard-tokenizer
							     :input string))))


(defclass per-field-analyzer-wrapper (analyzer)
  ((default-analyzer :initarg :default-analyzer)
   (analyzers :initform (make-hash-table :test #'equal))))

(defmethod token-stream ((self per-field-analyzer-wrapper) field string)
  (with-slots (analyzers default-analyzer) self
    (let ((analyzer (gethash field analyzers default-analyzer)))
      (token-stream analyzer field string))))
