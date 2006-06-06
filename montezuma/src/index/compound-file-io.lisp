(in-package #:montezuma)


(defclass compound-file-reader-file-entry ()
  ((offset :initarg :offset :reader offset)
   (size :accessor size)))

(defclass compound-file-reader (directory)
  ((directory :initarg :directory)
   (file-name :initarg :file-name)
   (stream)
   (entries :initform (make-table :test #'equal))))

(defmethod initialize-instance :after ((self compound-file-reader) &key)
  (with-slots (stream directory file-name entries) self
    (let ((success NIL))
      (unwind-protect 
	   (progn
	     (setf stream (open-input directory file-name))
	     (let ((count (read-vint stream))
		   (entry nil))
	       (dotimes (i count)
		 (let ((offset (read-long stream))
		       (id (read-string stream)))
		   (unless (null entry)
		     (setf (size entry) (- offset (offset entry))))
		   (setf entry (make-instance 'compound-file-reader-file-entry
					      :offset offset))
		   (setf (table-value entries id) entry)))
	       (unless (null entry)
		 (setf (size entry) (- (size stream) (offset entry))))
	       (setf success T)))
	(unless (or success (null stream))
	  ;; FIXME: Ferret has a "rescue IOError" here; what does that do?
	  (close stream))))))

(defmethod close ((self compound-file-reader))
  (with-slots (stream entries) self
    (when (null stream)
      (error "~S is already closed." self))
    (clrtable entries)
    (close stream)
    (setf stream nil)))

(defmethod open-input ((self compound-file-reader) id)
  (with-slots (stream entries) self
    (when (null stream)
      (error "Stream is closed for ~S." self))
    (let ((entry (table-value entries id)))
      (if (null entry)
	  (error "No sub-file with id ~S found in ~S" id self)
	  (make-instance 'cs-index-input
			 :base stream
			 :file-offset (offset entry)
			 :size (size entry))))))

(defmethod files ((self compound-file-reader))
  (with-slots (entries) self
    (table-keys entries)))

(defmethod file-count ((self compound-file-reader))
  (with-slots (entries) self
    (length (table-entries entries))))

(defmethod file-exists-p ((self compound-file-reader) name)
  (with-slots (entries) self
    (in-table-p entries name)))

(defmethod file-size ((self compound-file-reader) name)
  (with-slots (entries) self
    (size (table-value entries name))))

(defmethod modified-time ((self compound-file-reader) name)
  (with-slots (directory) self
    (modified-time directory name)))

(defmethod touch ((self compound-file-reader) name)
  (with-slots (directory) self
    (touch directory name)))

(defmethod delete-file ((self compound-file-reader) name)
  (declare (ignore name))
  (error "~S does not support ~S." 'compound-file-reader 'delete-file))

(defmethod rename-file ((self compound-file-reader) from to)
  (declare (ignore from to))
  (error "~S does not support ~S." 'compound-file-reader 'rename-file))
  
(defmethod create-output ((self compound-file-reader) name)
  (declare (ignore name))
  (error "~S does not support ~S." 'compound-file-reader 'create-output))

(defmethod make-lock ((self compound-file-reader) lock-name)
  (declare (ignore lock-name))
  (error "~S does not support ~S." 'compound-file-reader 'make-lock))


(defclass cs-index-input (buffered-index-input)
  ((base :initarg :base)
   (file-offset :initarg :file-offset)
   (size :initarg :size :reader size)))

(defmethod initialize-instance :after ((self cs-index-input) &key)
  (assert (slot-boundp self 'base))
  (assert (slot-boundp self 'file-offset))
  (assert (slot-boundp self 'size)))
  
(defmethod close ((self cs-index-input))
  )

(defmethod read-internal ((self cs-index-input) b offset len)
  (with-slots (base size file-offset) self
    (let ((start (pos self)))
      (when (> (+ start len) size)
	(error "Read past EOF on ~S." self))
      (seek base (+ file-offset start))
      (read-bytes base b offset len))))

(defmethod seek-internal ((self cs-index-input) pos)
  (declare (ignore pos))
  )


(defclass compound-file-writer-file-entry ()
  ((file-name :initarg :file-name :reader file-name)
   (dir-offset :accessor dir-offset)
   (data-offset :accessor data-offset)))

(defclass compound-file-writer ()
  ((directory :initarg :directory)
   (file-name :initarg :file-name)
   (ids :initform '())
   (file-entries :initform '())
   (merged-p :initform NIL)))


(defgeneric add-file (compound-file-writer file-name))

(defmethod add-file ((self compound-file-writer) file-name)
  (with-slots (merged-p ids file-entries) self
    (when merged-p
      (error "Extensions cannot be added after ~S has been merged." self))
    (if (member file-name ids :test #'equal)
	(error "File ~S already added to ~S." file-name self)
	(push file-name ids))
    (setf file-entries (append file-entries
			       (list
				(make-instance 'compound-file-writer-file-entry
					       :file-name file-name))))))

(defmethod close ((self compound-file-writer))
  (with-slots (merged-p file-name file-entries directory) self
    (when merged-p
      (error "Cannot close ~S because it has already been merged." self))
    (when (null file-entries)
      (error "No entries to merge have been defined for ~S." self))
    (setf merged-p T)
    (let ((os (create-output directory file-name)))
      (unwind-protect
	   (progn
	     (write-vint os (length file-entries))
	     (dolist (entry file-entries)
	       (setf (dir-offset entry) (pos os))
	       (write-long os 0)
	       (write-string os (namestring (file-name entry))))
	     (dolist (entry file-entries)
	       (setf (data-offset entry) (pos os))
	       (copy-file self entry os))
	     (dolist (entry file-entries)
	       (seek os (dir-offset entry))
	       (write-long os (data-offset entry))))
	(close os)))))

(defgeneric copy-file (compound-file-writer source os))

(defmethod copy-file ((self compound-file-writer) source os)
  (with-slots (directory) self
    (let ((is (open-input directory (file-name source))))
      (unwind-protect
	   (let ((start-ptr (pos os))
		 (remainder (size is))
		 (length (size is))
		 (buffer (make-array *default-buffer-size*)))
	     (while (> remainder 0)
	       (let ((len (min remainder *default-buffer-size*)))
		 (read-bytes is buffer 0 len)
		 (write-bytes os buffer len)
		 (decf remainder len)))
	     (unless (= remainder 0)
	       (error "Non-zero remainder after copying."))
	     (let* ((end-ptr (pos os))
		    (diff (- end-ptr start-ptr)))
	       (when (not (= diff length))
		 (error "Different in the input file offsets does not match the original file length."))))
	(close is)))))

      
		 
	     
