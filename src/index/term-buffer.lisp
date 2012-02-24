(in-package #:montezuma)

(defclass term-buffer ()
  ((text-buf :initform (make-adjustable-string 0))
   (text-length :initform -1 :reader text-length)
   (field :initform nil :reader field)
   (term :initform nil)
   (text-cache :initform nil)))

(defmethod print-object ((self term-buffer) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (field text-buf text-length) self
      (format stream "field:~S text:~S"
	      field 
	      (if (not (< text-length 0))
		  (subseq text-buf 0 text-length)
		  nil)))))

(defmethod initialize-copy :after ((self term-buffer) other)
  (set-from-term-buffer self other))


(defgeneric text (term-buffer))

(defmethod text ((self term-buffer))
  (with-slots (text-cache text-buf text-length) self
    (if text-cache
	text-cache
	(setf text-cache (subseq text-buf 0 text-length)))))

(defgeneric read-term-buffer (term-buffer input field-infos))

(defmethod read-term-buffer ((self term-buffer) input field-infos)
  (with-slots (term text-buf text-cache text-length field) self
    (setf term nil)
    (let* ((start (read-vint input))
	   (length (read-vint input))
	   (total-length (+ start length)))
      (ensure-text-buf-length self total-length)
      (let ((buf (string-to-bytes text-buf :start 0 :end total-length)))
	(read-chars input buf start length)
	(let ((s (bytes-to-string buf)))
          (setf text-length (length s))
	  (setf text-buf (make-adjustable-string (length s) s))))
      (setf field (field-name (get-field field-infos (read-vint input)))))
    (setf text-cache nil))
  self)

(defgeneric ensure-text-buf-length (term-buffer len))

(defmethod ensure-text-buf-length ((self term-buffer) len)
  (with-slots (text-buf text-cache) self
    (unless (>= (length text-buf) len)
      (let ((new-buf (make-adjustable-string (+ len 10))))
	(replace new-buf text-buf :start2 0 :end2 (length text-buf))
	(setf text-cache nil)
	(setf text-buf new-buf)))))

(defmethod reset ((self term-buffer))
  (with-slots (field text-buf text-cache text-length term) self
    (setf field nil
	  text-buf (make-adjustable-string 0)
	  text-length 0
	  text-cache nil
	  term nil)))

(defgeneric (setf term) (term term-buffer))
(defmethod (setf term) (term (self term-buffer))
  (if (null term)
      (progn (reset self) nil)
      (with-slots (text-buf text-cache text-length field) self
	(setf text-buf (make-adjustable-string (length (term-text term)) (term-text term)))
	(setf text-length (length text-buf))
	(setf text-cache nil)
	(setf field (term-field term))
	(setf (slot-value self 'term) term))))
      

(defgeneric to-term (term-buffer))

(defmethod to-term ((self term-buffer))
  (with-slots (field term text-buf text-length) self
    (if (null field)
	nil
	(if (not (null term))
	    term
	    (setf term (make-term field (subseq text-buf 0 text-length)))))))

(defmethod term ((self term-buffer))
  (to-term self))

(defun term-buffer-compare (tb1 tb2)
  (let ((f1 (field tb1))
	(f2 (field tb2)))
    (if (string= f1 f2)
	(string-compare (slot-value tb1 'text-buf)
			(slot-value tb2 'text-buf)
			:start1 0 :end1 (slot-value tb1 'text-length)
			:start2 0 :end2 (slot-value tb2 'text-length))
	(string-compare f1 f2))))

(defun term-buffer> (tb1 tb2)
  (let ((f1 (field tb1))
	(f2 (field tb2)))
    (if (string= f1 f2)
	(string> (slot-value tb1 'text-buf)
		 (slot-value tb2 'text-buf)
		 :start1 0 :end1 (slot-value tb1 'text-length)
		 :start2 0 :end2 (slot-value tb2 'text-length))
	(string> f1 f2))))

(defun term-buffer< (tb1 tb2)
  (let ((f1 (field tb1))
	(f2 (field tb2)))
  (if (string= f1 f2)
      (string< (slot-value tb1 'text-buf)
	       (slot-value tb2 'text-buf)
	       :start1 0 :end1 (slot-value tb1 'text-length)
	       :start2 0 :end2 (slot-value tb2 'text-length))
      (string< f1 f2))))

(defun term-buffer= (tb1 tb2)
  (let ((len1 (slot-value tb1 'text-length))
	(len2 (slot-value tb2 'text-length)))
    (and (= len1 len2)
	 (string= (slot-value tb1 'text-buf)
		  (slot-value tb2 'text-buf)
		  :start1 0 :end1 len1
		  :start2 0 :end2 len2)
	 (string= (field tb1) (field tb2)))))

(defgeneric set-from-term-buffer (term-buffer other))
(defmethod set-from-term-buffer ((self term-buffer) other)
  (with-slots (text-length text-buf text-cache field term) self
    (setf text-length (slot-value other 'text-length))
    (setf text-cache nil)
    (when (slot-value other 'text-buf)
      (setf text-buf (clone (slot-value other 'text-buf))))
    (setf field (slot-value other 'field))
    (setf term (slot-value other 'term))))


(defmethod term-compare ((t1 term) (t2 term-buffer))
  (term-compare t1 (to-term t2)))

(defmethod term= ((t1 term) (t2 term-buffer))
  (and (string= (term-text t1) (slot-value t2 'text-buf)
		:start2 0 :end2 (slot-value t2 'text-length))
       (string= (term-field t1) (field t2))))
		  
