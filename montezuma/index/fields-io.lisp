(in-package #:montezuma)

(defparameter *fdt-extension* "fdt")
(defparameter *fdx-extension* "fdx")

(defparameter +field-is-tokenized-bit+ 0)
(defparameter +field-is-binary-bit+ 1)
(defparameter +field-is-compressed-bit+ 2)


(defclass fields-reader ()
  ((field-infos :initarg :field-infos)
   (fields-stream)
   (index-stream)
   (size :reader size)))

(defmethod initialize-instance :after ((self fields-reader) &key directory segment)
  (with-slots (fields-stream index-stream size) self
    (setf fields-stream (open-segment-file directory segment *fdt-extension* :input))
    (setf index-stream (open-segment-file directory segment *fdx-extension* :input))
    (setf size (floor (size index-stream) 8))))

(defmethod close ((self fields-reader))
  (with-slots (fields-stream index-stream) self
    (close fields-stream)
    (close index-stream)))

(defmethod get-doc ((self fields-reader) n)
  (with-slots (index-stream fields-stream field-infos) self
    (seek index-stream (* n 8))
    (let ((position (read-long index-stream)))
      (seek fields-stream position))
    (let ((doc (make-instance 'document)))
      (let ((num-fields (read-vint fields-stream)))
	(dotimes (i num-fields)
	  (let* ((field-number (read-vint fields-stream))
		 (fi (get-field field-infos field-number))
		 (bits (read-byte fields-stream)))
	    (let ((compressed (logbitp +field-is-compressed-bit+ bits))
		  (tokenize (logbitp +field-is-tokenized-bit+ bits))
		  (binary (logbitp +field-is-binary-bit+ bits)))
	      (if binary
		  (let ((b (make-array (read-vint fields-stream))))
		    (read-bytes fields-stream b 0 (length b))
		    (if compressed
			(add-field doc
				   (make-binary-field (field-name fi)
						      (uncompress b)
						      :compress))
			(add-field doc (make-binary-field (field-name fi) b T))))
		  (let ((store T)
			(index (if (field-indexed-p fi)
				   (if tokenize
				       :tokenized
				       (if (field-omit-norms-p fi)
					   :no-norms
					   :untokenized))
				   NIL))
			(data nil))
		    (if compressed
			(progn
			  (setf store :compress)
			  (setf b (make-array (read-vint fields-stream)))
			  (read-bytes fields-stream b 0 (length b))
			  (setf data (coerce (uncompress b) 'string)))
			(setf data (read-string fields-stream)))
		    (let ((stv (if (field-store-term-vector-p fi)
				   (cond ((and (field-store-positions-p fi)
					       (field-store-offsets-p fi))
					  :with-positions-offsets)
					 ((field-store-positions-p fi) :with-positions)
					 ((field-store-offsets-p fi) :with-offsets)
					 (T T))
				   NIL)))
		      (add-field doc (make-field (field-name fi) data
						 :stored store
						 :index index
						 :store-term-vector stv)))))))))
      doc)))

				     
(defun uncompress (input)
  ;; FIXME: well, yes.
  input)


(defclass fields-writer ()
  ((field-infos :initarg :field-infos)
   (fields-stream)
   (index-stream)))


(defun open-segment-file (directory segment extension direction)
  (check-type direction (member :input :output))
  (let ((file (add-file-extension segment extension)))
    (ecase direction
      (:input (open-input directory file))
      (:output (create-output directory file)))))


(defmethod initialize-instance :after ((self fields-writer) &key directory segment)
  (with-slots (fields-stream index-stream) self
    (setf fields-stream (open-segment-file directory segment *fdt-extension* :output))
    (setf index-stream (open-segment-file directory segment *fdx-extension* :output))))

(defmethod close ((self fields-writer))
  (with-slots (fields-stream index-stream) self
    (close fields-stream)
    (close index-stream)))

(defmethod add-document ((fields-writer fields-writer) document)
  (with-slots (index-stream fields-stream field-infos) fields-writer
    (write-long index-stream (pos fields-stream))
    (write-vint fields-stream (count-if #'field-stored-p (all-fields document)))
    (dolist (field (all-fields document))
      (when (field-stored-p field)
	(write-vint fields-stream (get-field-number field-infos (field-name field)))
	(let ((bits 0))
	  (flet ((turn-on (bit)
		   (setf bits (logior bits (expt 2 bit)))))
	    (when (field-tokenized-p field) (turn-on +field-is-tokenized-bit+))
	    (when (field-binary-p field) (turn-on +field-is-binary-bit+))
	    (when (field-compressed-p field) (turn-on +field-is-compressed-bit+)))
	  (write-byte fields-stream bits))
	(if (field-compressed-p field)
	    (let ((data (if (field-binary-p field)
			    (compress (binary-value field))
			    (compress (string-value field)))))
	      (save-data fields-writer data))
	    (if (field-binary-p field)
		(save-data fields-writer (binary-value field))
		(write-string fields-stream (string-value field))))))))

(defun compress (input)
  ;; FIXME: uh huh.
  input)

(defmethod save-data ((self fields-writer) data)
  (with-slots (fields-stream) self
    (let ((len (length data)))
      (write-vint fields-stream len)
      (write-bytes fields-stream data len))))

