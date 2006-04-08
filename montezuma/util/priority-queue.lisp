(in-package #:montezuma)

;;;
;;; Heap (for the priority queue)
;;;

(defun heap-parent (i)
  (ash i -1))


(defun heap-left (i)
  (ash i 1))


(defun heap-right (i)
  (1+ (ash i 1)))


(defun heap-size (heap)
  (1- (length heap)))


(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (heap-size heap))
          largest)
      (setf largest (if (and (<= l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (<= r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))
                                    


(defun heap-insert (heap new-item &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (incf (fill-pointer heap))
    (loop for i = (heap-size heap) then parent-i
          for parent-i = (heap-parent i)
          while (and (> i 0)
                     (not (ge (key (aref heap parent-i))
                              (key new-item))))
          do (setf (aref heap i) (aref heap parent-i))
          finally (setf (aref heap i) new-item))
    heap))
    


(defun heap-maximum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))


(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (when (< (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (heap-size heap)))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))


(defun heap-extract-maximum (heap &key (key #'identity) (test #'>=))
  (heap-extract heap 0 :key key :test test))


;;;
;;; Priority queue
;;;

(defclass priority-queue ()
  ((contents)
   (max-size :initarg :max-size)
   (predicate :initarg :predicate)
   (element-type :initarg :element-type :initform T)))

(defmethod initialize-instance :after ((queue priority-queue) &key)
  (initialize-heap queue))

(defmethod initialize-heap ((queue priority-queue))
  (with-slots (contents max-size) queue
    (setf contents (make-array max-size
			       :adjustable T
			       :fill-pointer 0))))

(defmethod less-than ((queue priority-queue) a b)
  (with-slots (predicate) queue
    (funcall predicate a b)))

(defmethod size ((queue priority-queue))
  (length (slot-value queue 'contents)))

(defmethod queue-pop ((queue priority-queue))
  (with-slots (contents) queue
    (if (zerop (length contents))
	nil
	(heap-extract-maximum contents :test #'(lambda (a b) (less-than queue a b))))))

(defmethod queue-push ((queue priority-queue) new-item)
  (with-slots (contents) queue
    (heap-insert contents new-item :test #'(lambda (a b) (less-than queue a b)))))

(defmethod queue-top ((queue priority-queue))
  (with-slots (contents) queue
      (if (zerop (length contents))
	  nil
	  (heap-maximum contents))))

(defmethod adjust-top ((queue priority-queue))
  (let ((top (queue-pop queue)))
    (queue-push queue top)))

(defmethod queue-clear ((queue priority-queue))
  (let* ((contents (slot-value queue 'contents))
	 (size (length contents)))
    (dotimes (i size)
      (setf (aref contents i) nil))
    (setf (fill-pointer contents) 0)))


(defmethod check-queue ((self priority-queue) when)
  (with-slots (contents element-type) self
    (assert (every #'(lambda (elt) (typep elt element-type))
		   contents))
      (when (> (length contents) 0)
	(let ((min (reduce #'(lambda (&optional a b)
			       (if (or (null b) (less-than self a b))
				   a
				   b))
			   contents)))
	  (assert (not (less-than self min (queue-top self))) ()
		  "Queue min is ~S, top is ~S [~S ~S] (~S)"
		  min (queue-top self)
		  (less-than self min (queue-top self))
		  (less-than self (queue-top self) min)
		  when)))))
		  
