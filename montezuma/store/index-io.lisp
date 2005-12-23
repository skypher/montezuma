(in-package #:montezuma)

(defclass index-input ()
  ())

(defgeneric read-byte (index-input))

(defgeneric read-bytes (index-input buffer offset length))

(defmethod read-int ((self index-input))
  (let ((i1 (read-byte self))
	(i2 (read-byte self))
	(i3 (read-byte self))
	(i4 (read-byte self)))
    (+ (if (zerop (logand i1 #x80)) 0 (- #x100000000))
       (ash i1 24) (ash i2 16) (ash i3 8) i4)))

(defmethod read-long ((self index-input))
  (+ (ash (read-int self) 32)
     (logand (read-int self) #xFFFFFFFF)))

(defmethod read-uint ((self index-input))
  (logior (ash (read-byte self) 24)
	  (ash (read-byte self) 16)
	  (ash (read-byte self) 8)
	  (read-byte self)))

(defmethod read-ulong ((self index-input))
  (logior (ash (read-uint self) 32)
	  (logand (read-uint self) #xFFFFFFFF)))


(defmethod read-vint ((self index-input))
  (let* ((b (read-byte self))
	 (i (logand b #x7f))
	 (shift 7))
    (loop while (not (zerop (logand b #x80)))
	 do
	 (setf b (read-byte self))
	 (setf i (logior i (ash (logand b #x7f) shift)))
	 (incf shift 7))
    i))

(defmethod read-vlong ((self index-input))
  (read-vint self))


(defmethod read-string ((self index-input))
  (let* ((length (read-vint self))
	 (chars (make-string length)))
    (read-chars self chars 0 length)
    (string chars)))

(defmethod read-chars ((self index-input) buffer start length)
  (dotimes (i length)
    (setf (elt buffer (+ i start)) (read-byte self))))

(defgeneric pos (index-input))

(defgeneric seek (index-input pos))

(defgeneric size (index-input))



(defclass index-output ()
  ())

(defgeneric write-byte (index-output byte))

(defgeneric write-bytes (index-output buffer length))

(defmethod write-int ((self index-output) i)
  (flet ((sa (i count) (logand (ash i count) #xff)))
    (write-byte self (sa i -24))
    (write-byte self (sa i -16))
    (write-byte self (sa i -8))
    (write-byte self (sa i 0))))

(defmethod write-uint ((self index-output) i)
  (write-int self i))

(defmethod write-vint ((self index-output) int)
  (loop for i = int then (ash i -7)
       while (> i 127) do (write-byte self (logior (logand i #x7f) #x80))
       finally (write-byte self i)))

#||
(defmethod write-vint ((self index-output) i)
  (while (> i 127)
    (write-byte self (logior (logand i #x7f) #x80))
    (setf i (ash i -7)))
  (write-byte self i))
||#

(defmethod write-vlong ((self index-output) i)
  (write-vint self i))

(defmethod write-long ((self index-output) i)
  (write-int self (ash i -32))
  (write-int self i))

(defmethod write-ulong ((self index-output) i)
  (write-long self i))

(defmethod write-string ((self index-output) s)
  (write-vint self (length s))
  (write-chars self s 0 (length s)))

(defmethod write-chars ((self index-output) buffer start length)
  (dotimes (i length)
    (write-byte self (aref buffer (+ i start)))))

(defgeneric flush (index-output))

(defgeneric close (index-output))

(defgeneric pos (index-output))

(defgeneric seek (index-output pos))

(defgeneric size (index-output))
