(in-package #:montezuma)


(defparameter *default-buffer-size* 10)


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
  (assert (typep b '(unsigned-byte 8)))
  (with-slots (buffer buffer-size buffer-position) self
    (when (>= buffer-position buffer-size)
      (flush self))
    (setf (aref buffer buffer-position) b)
    (incf buffer-position)))

(defmethod write-bytes ((self buffered-index-output) buffer length)
  (dotimes (i length)
    (write-byte self (aref buffer i))))

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
  (with-slots (buffer-start) self
    (setf buffer-start pos)))

(defgeneric flush-buffer (buffered-index-output buffer length))


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


(defun clone (object)
  (let ((clone (clone-object object)))
    (initialize-copy clone object)
    clone))

(defmethod clone-object ((object T))
  (make-instance (class-of object)))


(defmethod initialize-copy (self o)
  (declare (ignore self) (ignore o)))

(defmethod initialize-copy :after ((self buffered-index-input) o)
  (with-slots (buffer buffer-size buffer-start buffer-length buffer-position) self
    (let ((b (slot-value o 'buffer)))
      (setf buffer (make-array (length b) :initial-contents b)))
    (setf buffer-size (slot-value o 'buffer-size))
    (setf buffer-start (slot-value o 'buffer-start))
    (setf buffer-length (slot-value o 'buffer-length))
    (setf buffer-position (slot-value o 'buffer-position))))

(defmethod read-byte ((self buffered-index-input))
  (with-slots (buffer-position buffer-length buffer) self
    (when (>= buffer-position buffer-length)
      (refill self))
    (prog1 (aref buffer buffer-position)
      (incf buffer-position))))

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
  (with-slots (buffer-start buffer-length buffer-position) self
    (if (and (> pos buffer-start)
	     (< pos (+ buffer-start buffer-length)))
	(setf buffer-position (- pos buffer-start))
	(progn
	  (setf buffer-start pos)
	  (setf buffer-position 0)
	  (setf buffer-length 0)
	  (seek-internal self pos)))))

(defgeneric read-internal (buffered-index-input buffer offset length))

(defgeneric seek-internal (buffered-index-input pos))

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
