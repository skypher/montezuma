(in-package #:montezuma)

(defclass priority-queue ()
  ((size :initform 0 :reader size)
   (heap)
   (predicate :initarg :predicate)
   (max-size :initarg :max-size)))

(defmethod initialize-instance :after ((queue priority-queue) &key)
  (with-slots (heap max-size) queue
    (setf heap (make-array (+ max-size 1)))))

(defmethod print-object ((self priority-queue) stream)
  (print-unreadable-object (self stream :identity T :type T)
    (with-slots (size heap) self
      (format stream "~W" (subseq heap 1 (+ size 1))))))

(defmethod less-than ((queue priority-queue) a b)
  (with-slots (predicate) queue
    (funcall predicate a b)))


(defmethod queue-push ((queue priority-queue) object)
  (with-slots (size heap) queue
    (incf size)
    (setf (aref heap size) object)
    (up-heap queue)))

(defmethod queue-insert ((queue priority-queue) object)
  (with-slots (size max-size heap) queue
    (cond ((< size max-size)
	   (queue-push queue object)
	   T)
	  ((and (> size 0) (less-than queue (queue-top queue) object))
	   (setf (aref heap 1) object)
	   (down-heap queue)
	   T)
	  (T NIL))))

(defmethod queue-top ((queue priority-queue))
  (with-slots (heap) queue
    (aref heap 1)))

(defmethod queue-pop ((queue priority-queue))
  (with-slots (size heap) queue
    (if (> size 0)
	(let ((result (aref heap 1)))
	  (setf (aref heap 1) (aref heap size)
		(aref heap size) nil)
	  (decf size)
	  (down-heap queue)
	  (values result T))
	(values nil NIL))))

(defmethod queue-clear ((queue priority-queue))
  (with-slots (size heap) queue
    (dotimes (i (+ size 1))
      (setf (aref heap i) nil))
    (setf size 0)))

(defmethod adjust-heap ((queue priority-queue))
  (down-heap queue))

(defmethod up-heap ((queue priority-queue))
  (with-slots (size heap) queue
    (let* ((i size)
	   (node (aref heap i))
	   (j (floor i 2)))
      (while (and (> j 0) (less-than queue node (aref heap j)))
	(setf (aref heap i) (aref heap j)
	      i j)
	(setf j (floor j 2)))
      (setf (aref heap i) node))))

(defmethod down-heap ((queue priority-queue))
  (with-slots (size heap) queue
    (let* ((i 1)
	   (node (aref heap i))
	   (j (* i 2))
	   (k (+ j 1)))
      (when (and (<= k size) (less-than queue (aref heap k) (aref heap j)))
	(setf j k))
      (while (and (<= j size) (less-than queue (aref heap j) node))
	(setf (aref heap i) (aref heap j)
	      i j)
	(setf j (* i 2))
	(setf k (+ j 1))
	(when (and (< k size) (less-than queue (aref heap k) (aref heap j)))
	  (setf j k)))
      (setf (aref heap i) node))))


    
	  
