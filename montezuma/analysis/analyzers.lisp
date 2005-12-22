(in-package #:montezuma)

(defclass analyzer ()
  ())

(defmethod token-stream ((self analyzer) field string)
  (declare (ignore field))
  (make-instance 'lowercase-tokenizer :input string))

(defmethod position-increment-gap ((self analyzer) field-name)
  (declare (ignore field-name))
  0)


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

