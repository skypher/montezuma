(in-package montezuma)

(defparameter +score-cache-size+ 32)

(defclass term-scorer (scorer)
  ((document :reader document :initform 0)
   (documents :initform (make-term-scorer-array) :reader documents)
   (freqs :initform (make-term-scorer-array) :reader freqs)
   (pointer :initform 0 :accessor pointer)
   (pointer-max :initform 0 :accessor pointer-max)
   (score-cache :initform (make-term-scorer-array) :accessor score-cache)
   (weight :initarg :weight)
   (term-docs :initarg :term-docs :reader term-docs)
   (norms :initarg :norms :reader norms)
   (weight-value :reader weight-value)))

(defun make-term-scorer-array ()
  (make-array +score-cache-size+ :adjustable t
              :initial-element 0))

(defmethod initialize-instance :after ((self term-scorer) &key)
  (setf (slot-value self 'weight-value) 
        (value (slot-value self 'weight)))
  (dotimes (i +score-cache-size+)
    (setf (aref (slot-value self 'score-cache) i)
          (* (weight-value self) (tf (similarity self) i)))))

(defmethod each-hit ((self term-scorer) fn)
  (loop while (next? self) do
        (let* ((f (aref (freqs self) (pointer self)))
               (score (if (< f +score-cache-size+)
                        (aref (score-cache self) f)
                        ;; cache miss
                        (* (weight-value self) 
                           (tf (similarity self) f)))))
          (setf score (* score (similarity-decode-norm 
                                (aref (norms self) (document self)))))
          (funcall fn (document self) score))))

(defmethod each-hit-up-to ((self term-scorer) (max-docs integer) fn)
  ;;??
  )

(defmethod next? ((self term-scorer))
  (incf (pointer self))
  (when (>= (pointer self) (pointer-max self))
    (setf (pointer-max self) 
          (read-segment-term-doc-enum
           (term-docs self) (documents self) (freqs self)))
    (cond ((zerop (pointer-max self))
           (close (term-docs self))
           (setf (slot-value self 'document) +max-docs+)
           (return-from next? nil))
          (T
           (setf (pointer self) 0))))
  (setf (slot-value self 'document) (aref (documents self) (pointer self))))


(defmethod score ((self term-scorer))
  (let* ((f (aref (freqs self) (pointer self)))
         (raw (if (< f +score-cache-size+)
                (aref (slot-value self 'score-cache) f)
                (* (weight-value self) (tf (similarity self) f)))))
    
    ;; normalize for field
    (values (* raw (similarity-decode-norm (aref (norms self) (document self)))))))

(defmethod skip-to ((self term-scorer) target)
  ;; First scan in cache.
  (while (< (incf (pointer self)) (pointer-max self))
    (when (>= (aref (documents self) (pointer self)) target)
      (setf (slot-value self 'document) (aref (documents self) (pointer self)))
      (return-from skip-to T)))
  ;; Not found in cache, seek underlying stream.
  (with-slots (term-docs document documents freqs pointer-max pointer) self
    (let ((result (skip-to term-docs target)))
      (if result
	  (progn
	    (setf pointer-max 1)
	    (setf pointer 0)
	    (setf document (doc term-docs))
	    (setf (aref documents pointer) document)
	    (setf (aref freqs pointer) (freq term-docs)))
	  (setf document +max-docs+))
      result)))


  
(defmethod explain (document)
  ;;??
  )
