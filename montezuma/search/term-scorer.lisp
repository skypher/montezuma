(in-package montezuma)

(defparameter +score-cache-size+ 32)

(defclass term-scorer (scorer)
  ((document :reader document :initform 0)
   (documents :initform (make-term-scorer-array))
   (freqs :initform (make-term-scorer-array))
   (pointer :initform 0)
   (pointer-max :initform 0)
   (score-cache :initform (make-term-scorer-array))
   (weight :initarg :weight)
   (term-docs :initarg :term-docs)
   (norms :initarg :norms)
   (weight-value :reader weight-value)))

(defun make-term-scorer-array ()
  (make-array +score-cache-size+ :fill-pointer 0 :adjustable t
              :initial-element 0))

(defmethod initialize-instance :after ((self term-scorer) &key)
  (setf (slot-value self 'weight-value) 
        (value weight))
  (dotimes (i +score-cache-size+)
    (setf (aref (score-cache self) i)
          (* (weight-value self) (tf (similarity self) i)))))

(defmethod each-hit ((self term-scorer))
  ;;??
  )

(defmethod each-hit-up-to ((self term-scorer) (max-docs integer))
  ;;??
  )

(defmethod next? ((self term-scorer))
  (incf (pointer self))
  (when (> (pointer self) (pointer-max self))
    (setf (pointer-max self) (read (term-docs self) (documents self) (freqs self)))
    (cond ((zerop (pointer-max self))
           (close (term-docs self))
           (setf (document self) +max-docs+)
           (values nil))
          (t
           (setf (pointer self) 0)))
    (setf (document self) (aref (documents self) (pointer self)))
    (values t)))

(defmethod score ((self term-scorer))
  (let* ((f (aref (freqs self) (pointer self)))
         (raw (if (< f +score-cache-size+)
                (aref (score-cache self) f)
                (* (weight-value self) (tf (simularity self) f)))))
    
    ;; normalize for field
    (values (* raw (similarity-decode-norm (aref (norms self) (document self)))))))

(defmethod skip-to ((self term-scorer) target)
  (while (< (incf (pointer self)) (pointer-max self))
    (when (>= (aref (documents self) (pointer self)) target)
      (setf (document self) (aref (documents self) (pointer self)))
      (values t))))
  
(defmethod explain (document)
  ;;??
  )
