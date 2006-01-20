(in-package #:montezuma)

(defclass document-writer ()
  ((directory :initarg :directory)
   (analyzer :initarg :analyzer)
   (similarity :initarg :similarity)
   (max-field-length :initarg :max-field-length)
   (term-index-interval :initarg :term-index-interval)
   (posting-table :initform (make-hash-table :test #'eq))
   (term-buffer :initform (make-term "" "")))
  (:default-initargs
   :term-index-interval *default-term-index-interval*))


(defparameter *fnm-extension* (make-pathname :type "fnm"))
(defparameter *frq-extension* (make-pathname :type "frq"))
(defparameter *prx-extension* (make-pathname :type "prx"))

(defmethod add-document-to-writer ((self document-writer) segment document)
  (with-slots (field-infos directory posting-table field-lengths
			   field-positions field-offsets field-boosts) self
    (setf field-infos (make-instance 'field-infos))
    (add-document field-infos document)
    (write-to-dir field-infos directory (merge-pathnames *fnm-extension* segment))
    (let ((fields-writer (make-instance 'fields-writer
					:directory directory
					:segment segment
					:field-infos field-infos)))
      (unwind-protect
	   (add-document fields-writer document)
	(close fields-writer))
      (clrhash posting-table)
      (let ((arr-size (size field-infos)))
	(setf field-lengths (make-array arr-size :initial-element 0))
	(setf field-positions (make-array arr-size :initial-element 0))
	(setf field-offsets (make-array arr-size :initial-element 0))
	(setf field-boosts (make-array arr-size :initial-element (boost document)))
	(invert-document self document)
	(let ((postings (sort-posting-table self)))
	  (write-postings self postings segment))
	(write-norms self segment)))))

(defmethod invert-document ((self document-writer) doc)
  (with-slots (field-infos field-offsets field-positions analyzer max-field-length field-offsets
			   info-stream field-lengths field-boosts) self
    (let ((fields (all-fields doc)))
      (dolist (field fields)
	(let* ((field-name (field-name field))
	       (field-info (get-field field-infos field-name))
	       (field-number (field-number field-info)))
	  (let ((length (aref field-lengths field-number))
		(position (aref field-positions field-number))
		(offset (aref field-offsets field-number)))
	    (when (> length 0)
	      (incf position (position-increment-gap analyzer field-name)))

	    (if (field-indexed-p field)
		(if (not (field-tokenized-p field))
		    (let ((string-value (string-value field)))
		      (if (field-store-offsets-p field)
			  (progn
			    (add-position field-name
					  string-value
					  position
					  (make-instance 'term-vector-offset-info
							 :start-offset offset
							 :end-offset (+ offset (length string-value))))
			    (incf position))
			  (progn
			    (incf offset (length string-value))
			    (incf length 1))))
		    (let* ((reader (reader-value field))
			   (stream (token-stream analyzer field-name reader)))
		      (unwind-protect
			   (let ((last-token nil)
				 (token nil))
			     (while (setf token (next stream))
			       (incf position (- (token-position-increment token) 1))
			       (if (field-store-offsets-p field-info)
				   (progn
				     (add-position field-name
						   (term-text token)
						   position
						   (make-instance 'term-vector-offset-info
								  :start-offset (+ offset (token-start token))
								  :end-offset (+ offset (token-end token))))
				     (incf position))
				   (progn
				     (add-position field-name (term-text token) position nil)
				     (incf position)))
			       (setf last-token token)
			       (incf length)
			       (when (> length max-field-length)
				 (when info-stream
				   (format info-stream "max field length ~S reached, ignoring following tokens"
					   max-field-length)
				   (return))))
			     (when last-token
			       (incf offset (+ (token-end token) 1))))
			(close stream)))))
	    (setf (aref field-lengths field-number) length)
	    (setf (aref field-positions field-number) position)
	    (setf (aref field-boosts field-number)
		  (* (aref field-boosts field-number) (boost field)))
	    (setf (aref field-offsets field-number) offset)))))))


(defmethod add-position ((self document-writer) field text position tv-offset-info)
  (with-slots (term-buffer posting-table) self
    (setf (term term-buffer) (make-term field text))
    (let ((posting (gethash term-buffer posting-table)))
      (if posting
	  (let ((freq (freq posting)))
	    (setf (aref (positions posting) freq) position)
	    (setf (aref (offsets posting) freq) tv-offset-info)
	    ;; FIXME: the ferret code is weird here.
	    #||
	    (when tv-offset-info
	      (setf (aref (offsets posting) freq) tv-offset-info))
	    ||#
	    (setf (freq posting) (+ freq 1)))
	  (let ((term (make-term field text)))
	    (setf (gethash term posting-table) (make-instance 'posting
							      :term term
							      :position position
							      :tv-offset-info tv-offset-info)))))))

(defmethod sort-posting-table ((self document-writer))
  (with-slots (posting-table) self
    (let ((postings (coerce (loop for posting being the hash-values in posting-table
				 collect posting)
			    'vector)))
      (setf postings (sort postings #'< :key #'term)))))

(defmethod write-postings ((self document-writer) postings segment)
  (with-slots (directory field-infos term-index-interval) self
    (let ((freq nil)
	  (prox nil)
	  (tis-writer nil)
	  (tv-writer nil))
      (unwind-protect
	   (let ((tis-writer (make-instance 'term-infos-writer
					    :directory directory
					    :segment segment
					    :field-infos field-infos
					    :term-index-interval term-index-interval))
		 (ti (make-instance 'term-info))
		 (current-field nil))
	     (setf freq (open-segment-file directory segment *frq-extension* :output))
	     (setf prox (open-segment-file directory segment *prx-extension* :output))
	     (dolist (posting postings)
	       (set-values ti 1 (pos freq) (pos prox) -1)
	       (add-term tis-writer (term posting) ti)
	       (let ((posting-freq (freq posting)))
		 (if (= posting-freq -1)
		     (write-vint freq 1)
		     (progn
		       (write-vint freq 0)
		       (write-vint freq posting-freq)))
		 (let ((last-position 0))
		   (dolist (position (positions posting))
		     (write-vint prox (- position last-position))
		     (setf last-position position)))
		 (let ((term-field (field (term posting))))
		   (when (not (equal current-field term-field))
		     (setf current-field term-field)
		     (let ((fi (get-field field-infos current-field)))
		       (if (field-store-term-vector-p fi)
			   (progn
			     (when (null tv-writer)
			       (setf tv-writer (make-instance 'term-vectors-writer
							      :directory directory
							      :segment segment
							      :field-infos field-infos))
			       (open-document tv-writer))
			     (open-field tv-writer current-field))
			   (when tv-writer
			     (close-field tv-writer)))))
		   (when (and tv-writer (field-open-p tv-writer))
		     (add-term-to-term-vectors-writer (text (term posting))
						      posting-freq
						      (positions posting)
						      (offsets posting))))))
	     (when tv-writer
	       (close-document tv-writer))))
      (progn
	;; FIXME raise some exceptions somewhere?
	(close freq)
	(close prox)
	(close tis-writer)
	(close tv-writer)))))

(defmethod write-norms ((self document-writer) segment)
  (with-slots (directory field-infos field-boosts similarity field-lengths) self
    (dosequence (fi field-infos :index i)
      (when (and (field-indexed-p fi) (not (field-omit-norms-p fi)))
	(let ((norm (* (aref field-boosts i)
		       (length-norm similarity (name fi) (aref field-lengths i))))
	      (norms (open-segment-file directory segment (format nil ".f~S" i))))
	  (unwind-protect
	       (write-byte norms (encode-norm norm))
	    (close norms)))))))

(defclass posting ()
  ((term :initarg term :reader term)
   (freq :initform 1 :accessor freq)
   (positions :initarg :positions :reader positions)
   (offsets :initarg :offsets :reader offsets)))
