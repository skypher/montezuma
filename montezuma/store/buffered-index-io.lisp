(in-package #:montezuma)


(defparameter *default-buffer-size* 128)


(defclass buffered-index-output (index-output)
  ((buffer)
   (buffer-size :initarg :buffer-size :reader buffer-size)
   (buffer-start :initform 0)
   (buffer-position :initform 0))
  (:default-initargs
   :buffer-size *default-buffer-size*))

(defmethod initialize-instance :after ((self buffered-index-output) &key)
  (with-slots (buffer buffer-size) self
    (setf buffer (make-array (list buffer-size)))))

(defmethod write-byte ((self buffered-index-output) b)
;;  (assert (typep b '(unsigned-byte 8)))
  (with-slots (buffer buffer-size buffer-position) self
    (when (>= buffer-position buffer-size)
      (flush self))
    (setf (aref buffer buffer-position) b)
    (incf buffer-position)))

(defmethod write-byte ((self buffered-index-output) b)
;;  (assert (typep b '(unsigned-byte 8)))
  (when (>= (slot-value self 'buffer-position) (slot-value self 'buffer-size))
    (flush self))
  (let ((buffer-position (slot-value self 'buffer-position)))
    (setf (aref (slot-value self 'buffer) buffer-position) b)
    (setf (slot-value self 'buffer-position) (+ buffer-position 1))))

(defmethod write-bytes ((self buffered-index-output) buffer length)
;;  (format T "~&write-bytes len: ~S  buf-size: ~S buf-pos: ~S"
;;	  length (slot-value self 'buffer-size) (slot-value self 'buffer-position))
  (labels ((write-bytes-aux (start)
	     (when (< start length)
;;	       (format T "~&  aux: ~S" start)
	       (let* ((buffer-position (slot-value self 'buffer-position))
		      (free-len (- (slot-value self 'buffer-size) buffer-position)))
		 (if (not (> free-len 0))
		     (progn
;;		       (format T "~&  flushing")
		       (flush self)
		       (write-bytes-aux start))
		     (let ((num-bytes-to-copy (min free-len (- length start))))
		       (replace (slot-value self 'buffer) buffer
				:start1 buffer-position
				:end1 (+ buffer-position num-bytes-to-copy)
				:start2 start
				:end2 (+ start num-bytes-to-copy))
		       (incf (slot-value self 'buffer-position) num-bytes-to-copy)
		       (write-bytes-aux (+ start num-bytes-to-copy))))))))
    (write-bytes-aux 0)))


(defmethod flush ((self buffered-index-output))
  (with-slots (buffer buffer-position buffer-start) self
    (flush-buffer self buffer buffer-position)
    (incf buffer-start buffer-position)
    (setf buffer-position 0)))

(defmethod close ((self buffered-index-output))
  (flush self))

(defmethod pos ((self buffered-index-output))
  (with-slots (buffer-start buffer-position) self
    (+ buffer-start buffer-position)))

(defmethod seek ((self buffered-index-output) pos)
  (flush self)
  (prog1
      (with-slots (buffer-start) self
	(setf buffer-start pos))
    (assert (eql (pos self) pos))))

(defclass buffered-index-input (index-input)
  ((buffer)
   (buffer-size :initarg :buffer-size :reader buffer-size)
   (buffer-start :initform 0)
   (buffer-length :initform 0)
   (buffer-position :initform 0))
  (:default-initargs
   :buffer-size *default-buffer-size*))

(defmethod initialize-instance :after ((self buffered-index-input) &key)
  (with-slots (buffer buffer-size) self
    (setf buffer (make-array (list buffer-size)))))


(defmethod initialize-copy :after ((self buffered-index-input) o)
  (with-slots (buffer) self
    (let ((other-buffer (slot-value o 'buffer)))
    (when other-buffer
      (setf buffer (clone other-buffer))))))


#|
(defmethod read-byte ((self buffered-index-input))
  (with-slots (buffer-position buffer-length buffer) self
    (when (>= buffer-position buffer-length)
      (refill self))
    (prog1 (aref buffer buffer-position)
      (incf buffer-position))))
|#

(defmethod read-byte ((self buffered-index-input))
  (let ((buffer-position (slot-value self 'buffer-position))
	(buffer-length (slot-value self 'buffer-length)))
    (when (>= buffer-position buffer-length)
      (refill self)
      (setf buffer-position (slot-value self 'buffer-position)))
    (setf (slot-value self 'buffer-position)
	  (1+ buffer-position))
    (aref (slot-value self 'buffer) buffer-position)))

(defmethod read-bytes ((self buffered-index-input) buffer offset length)
  (with-slots (buffer-size buffer-start buffer-position buffer-length) self
    (if (< length buffer-size)
	(dotimes (i length)
	  (setf (aref buffer (+ i offset)) (read-byte self)))
	(let ((start (pos self)))
	  (seek-internal self start)
	  (read-internal self buffer offset length)
	  (setf buffer-start (+ start length))
	  (setf buffer-position 0)
	  (setf buffer-length 0))))
  buffer)

(defmethod pos ((self buffered-index-input))
  (with-slots (buffer-start buffer-position) self
    (+ buffer-start buffer-position)))

(defmethod seek ((self buffered-index-input) pos)
  (prog1
      (with-slots (buffer-start buffer-length buffer-position) self
	(if (and (> pos buffer-start)
		 (< pos (+ buffer-start buffer-length)))
	    (setf buffer-position (- pos buffer-start))
	    (progn
	      (setf buffer-start pos)
	      (setf buffer-position 0)
	      (setf buffer-length 0)
	      (seek-internal self pos))))
    (assert (eql (pos self) pos))))

(defmethod refill ((self buffered-index-input))
  (with-slots (buffer-start buffer-position buffer-size buffer-length buffer)
      self
    (let* ((start (+ buffer-start buffer-position))
	   (last (+ start buffer-size)))
      (when (> last (size self))
	(setf last (size self)))
      (setf buffer-length (- last start))
      (when (<= buffer-length 0)
	(error "EOF"))

      (when (null buffer)
	(setf buffer (make-string buffer-size)))

      (read-internal self buffer 0 buffer-length)

      (setf buffer-start start)
      (setf buffer-position 0))))
