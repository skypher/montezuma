(in-package #:montezuma)

;;(defvar *ram-id-counter* 0)

(defclass ram-directory (directory)
  ((dir :initarg :dir)
   (files :initform (make-hash-table :test #'equal))
;;   (id :initform (incf *ram-id-counter*))
   )
  (:default-initargs
   :dir nil))


(defmethod initialize-instance :after ((self ram-directory) &key (close-dir-p NIL))
;;  (format T "~&creating dir id ~S" (slot-value self 'id))
;;  (when (or (= (slot-value self 'id) 3) (= (slot-value self 'id) 2)) (break))
  (with-slots (dir) self
    (when dir
      (do-files (file dir)
	(let ((os (create-output self file))
	      (is (open-input dir file)))
	  (let* ((len (size is))
		 (buf (make-array (list len))))
	    (read-bytes is buf 0 len)
	    (write-bytes os buf len)
	    (close is)
	    (close os))))
      (if close-dir-p
	  (close dir)))))

(defmethod files ((self ram-directory))
  (let ((file-list '()))
    (with-slots (files) self
      (maphash #'(lambda (name file)
		   (declare (ignore file))
		   (push name file-list))
	       files))
    (reverse file-list)))

(defun normalize-file-name (name)
  (if (pathnamep name)
      (namestring name)
      name))

(defmethod file-exists-p ((self ram-directory) name)
  (with-slots (files) self
    (gethash (normalize-file-name name) files)))

(defmethod modified-time ((self ram-directory) name)
  (with-slots (files) self
    (mtime (gethash (normalize-file-name name) files))))

(defmethod touch ((self ram-directory) name)
  (setf name (normalize-file-name name))
  (with-slots (files) self
    (when (null (gethash name files))
      (setf (gethash name files) (make-instance 'ram-file :name name)))
    (setf (mtime (gethash name files)) (get-universal-time))))

(defmethod delete-file ((self ram-directory) name)
;;  (format T "~&~S" (list :delete (slot-value self 'id) name))
;;  (when (equal name "_3.tvf") (break))
  (with-slots (files) self
    (remhash (normalize-file-name name) files)))

(defmethod rename-file ((self ram-directory) from to)
;;  (format T "~&~S" (list :rename (slot-value self 'id) from to))
  (setf from (normalize-file-name from)
	to (normalize-file-name to))
  (with-slots (files) self
    (let ((file (gethash from files)))
      (setf (file-name file) to)
      (setf (gethash to files) file))
    (remhash from files)))

(defmethod file-size ((self ram-directory) name)
  (with-slots (files) self
    (size (gethash (normalize-file-name name) files))))

(defmethod create-output ((self ram-directory) name)
  (setf name (normalize-file-name name))
;;  (format T "~&~S" (list :create (slot-value self 'id) name))
  (with-slots (files) self
    (let ((file (make-instance 'ram-file :name name)))
      (setf (gethash name files) file)
      (make-instance 'ram-index-output :file file))))

(defmethod open-input ((self ram-directory) name)
  (setf name (normalize-file-name name))
  (with-slots (files) self
    (let ((file (gethash name files)))
      (unless file
	(error "File ~S does not exist." name))
      (assert (string= name (file-name file)))
      (make-instance 'ram-index-input :file file))))

(defmethod print-file ((self ram-directory) name)
  (with-slots (files) self
    (let* ((input (make-instance 'ram-index-input :file (gethash (normalize-file-name name) files)))
	   (buf (make-array (list (size input)))))
      (read-internal input buf 0 (size input))
      (format T "~A" buf))))

(defmethod make-lock ((self ram-directory) name)
  (with-slots (lock-prefix) self
    (make-instance 'ram-lock
		   :name (format nil "~A~A" lock-prefix (normalize-file-name name))
		   :dir self)))

(defmethod close ((self ram-directory))
  )


(defclass ram-index-output (buffered-index-output)
  ((file :initarg :file)
   (pointer :initform 0)))

(defmethod print-object ((self ram-index-output) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (file pointer) self
      (format stream "file:~S pointer:~S" file pointer))))

(defmethod size ((self ram-index-output))
  (with-slots (file) self
    (size file)))

(defmethod flush-buffer ((self ram-index-output) src len)
  (with-slots (file pointer) self
    (let* ((buffer-number (floor pointer (buffer-size self)))
	   (buffer-offset (mod pointer (buffer-size self)))
	   (bytes-in-buffer (- (buffer-size self) buffer-offset))
	   (bytes-to-copy (min bytes-in-buffer len)))
      (extend-buffer-if-necessary self buffer-number)
      (let ((buffer (elt (buffers file) buffer-number)))
	(replace buffer src
		 :start1 buffer-offset :end1 (+ buffer-offset bytes-to-copy)
		 :start2 0 :end2 (+ 0 bytes-to-copy))
	(when (< bytes-to-copy len)
	  (let ((src-offset bytes-to-copy))
	    (setf bytes-to-copy (- len bytes-to-copy))
	    (incf buffer-number)
	    (extend-buffer-if-necessary self buffer-number)
	    (setf buffer (aref (buffers file) buffer-number))
	    (replace buffer src
		     :start1 0 :end1 (+ 0 bytes-to-copy)
		     :start2 src-offset :end2 (+ src-offset bytes-to-copy))))
	(incf pointer len)
	(unless (< pointer (size file))
	  (setf (size file) pointer))
	(setf (mtime file) (get-universal-time))))))

(defmethod reset ((self ram-index-output))
  (seek self 0)
  (with-slots (file) self
    (setf (size file) 0)))

(defmethod seek :after ((self ram-index-output) pos)
  (with-slots (pointer) self
    (setf pointer pos)))

(defmethod close :after ((self ram-index-output))
  (with-slots (file) self
    (setf (mtime file) (get-universal-time))))
	   
(defmethod write-to ((self ram-index-output) output)
  (flush self)
  (let ((file (slot-value self 'file))
	(buffer-size (buffer-size self)))
    (let ((last-buffer-number (floor (size file) buffer-size))
	  (last-buffer-offset (mod (size file) buffer-size)))
      (dosequence (buffer (buffers file) :index i)
	(let ((len (if (= i last-buffer-number) last-buffer-offset buffer-size)))
	  (write-bytes output buffer len))))))

(defmethod make-new-buffer ((self ram-index-output))
  (make-array (list (buffer-size self))))

(defmethod extend-buffer-if-necessary ((self ram-index-output) buffer-number)
  (with-slots (file) self
    (let ((buffers (buffers file)))
      (when (= buffer-number (length buffers))
	(vector-push-extend (make-new-buffer self) buffers)))))


(defclass ram-index-input (buffered-index-input)
  ((file :initarg :file)
   (pointer :initform 0)
   (closed-p :initform NIL)))

(defmethod print-object ((self ram-index-input) stream)
  (print-unreadable-object (self stream :identity T :type T)
    (with-slots (file pointer) self
      (format stream "file:~S pointer:~S" file pointer))))

(defmethod initialize-copy :after ((self ram-index-input) o)
  (with-slots (file pointer) self
    (setf file (slot-value o 'file))
    (setf pointer (slot-value o 'pointer))))

(defmethod size ((self ram-index-input))
  (with-slots (file) self
    (size file)))


(defmethod read-internal ((self ram-index-input) b offset length)
  (with-slots (file pointer closed-p) self
    (when closed-p
      (error "~S cannot be read from because it has already been closed." self))
    (let ((remainder length)
	  (start pointer))
      (while (not (= remainder 0))
	(let* ((buffer-number (floor start (buffer-size self)))
	       (buffer-offset (mod start (buffer-size self)))
	       (bytes-in-buffer (- (buffer-size self) buffer-offset)))
	  (let ((bytes-to-copy (if (>= bytes-in-buffer remainder)
				   remainder
				   bytes-in-buffer)))
	    (let ((buffer (elt (buffers file) buffer-number))
		  (bo2 buffer-offset)
		  (do2 offset))
	      (replace b buffer
		       :start1 do2 :end1 (+ do2 bytes-to-copy)
		       :start2 bo2 :end2 (+ bo2 bytes-to-copy))
	      (incf offset bytes-to-copy)
	      (incf start bytes-to-copy)
	      (decf remainder bytes-to-copy)))))
      (incf pointer length))))

(defmethod seek-internal ((self ram-index-input) pos)
  (with-slots (pointer) self
    (setf pointer pos)))

(defmethod close ((self ram-index-input))
  (with-slots (closed-p) self
    (setf closed-p T)))


(defclass ram-file ()
  ((name :initarg :name :accessor file-name)
   (buffers :accessor buffers :initform (make-array (list 5) :fill-pointer 0 :adjustable T))
   (mtime :accessor mtime :initform (get-universal-time))
   (size :accessor size :initform 0)))

(defmethod print-object ((self ram-file) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "name:~S size:~S" (file-name self) (size self))))
