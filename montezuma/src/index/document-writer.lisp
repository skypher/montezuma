(in-package #:montezuma)

(defclass document-writer ()
  ((directory :initarg :directory)
   (analyzer :initarg :analyzer)
   (similarity :initarg :similarity)
   (max-field-length :initarg :max-field-length)
   (field-infos)
   (field-lengths)
   (field-positions)
   (field-offsets)
   (field-boosts)
   (term-index-interval :initarg :term-index-interval)
   (posting-table :initform (make-hash-table :test #'equal))
   (term-buffer :initform (make-term "" ""))
   (info-stream :initform nil :accessor info-stream))
  (:default-initargs
   :term-index-interval *index-writer-default-term-index-interval*))


(defparameter *fnm-extension* "fnm")
(defparameter *frq-extension* "frq")
(defparameter *prx-extension* "prx")

(defgeneric add-document-to-writer (document-writer segment document))

(defmethod add-document-to-writer ((self document-writer) segment document)
  (with-slots (field-infos directory posting-table field-lengths
			   field-positions field-offsets field-boosts) self
    (setf field-infos (make-instance 'field-infos))
    (add-doc-fields field-infos document)
    (write-to-dir field-infos directory (add-file-extension segment *fnm-extension*))
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

(defgeneric invert-document (document-writer doc))

(defmethod invert-document ((self document-writer) doc)
  (with-slots (field-infos field-offsets field-positions analyzer max-field-length
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
	    (when (field-indexed-p field-info)
	      (if (not (field-tokenized-p field))
		  (let ((string-value (string-value field)))
		    (if (field-store-offsets-p field-info)
			(progn
			  (add-position self 
					field-name
					string-value
					position
					(make-instance 'term-vector-offset-info
						       :start-offset offset
						       :end-offset (+ offset (length string-value))))
			  (incf position))
			(progn
			  (add-position self field-name string-value position nil)
			  (incf position)))
		    (incf offset (length string-value))
		    (incf length))
		  (let* ((reader (reader-value field))
			 (stream (token-stream analyzer field-name reader)))
		    (unwind-protect
			 (let ((last-token nil)
			       (token nil))
			   (while (setf token (next-token stream))
			     (incf position (- (token-increment token) 1))
			     (if (field-store-offsets-p field-info)
				 (progn
				   (add-position self
						 field-name
						 (term-text token)
						 position
						 (make-instance 'term-vector-offset-info
								:start-offset (+ offset (token-start token))
								:end-offset (+ offset (token-end token))))
				   (incf position))
				 (progn
				   (add-position self field-name (term-text token) position nil)
				   (incf position)))
			     (setf last-token token)
			     (incf length)
			     (when (> length max-field-length)
			       (when info-stream
				 (format info-stream "max field length ~S reached, ignoring following tokens"
					 max-field-length)
				 (return))))
			   (when last-token
			     (incf offset (+ (token-end last-token) 1))))
		      (close stream)))))
	    (setf (aref field-lengths field-number) length)
	    (setf (aref field-positions field-number) position)
	    (setf (aref field-boosts field-number)
		  (* (aref field-boosts field-number) (boost field)))
	    (setf (aref field-offsets field-number) offset)))))))


(defgeneric add-position (document-writer field text position tv-offset-info))

(defmethod add-position ((self document-writer) field text position tv-offset-info)
  (with-slots (term-buffer posting-table) self
    (set-term term-buffer field text)
    (let ((posting (gethash (posting-key term-buffer) posting-table)))
      (if posting
	  (let ((freq (freq posting)))
	    (vector-push-extend position (positions posting))
	    (vector-push-extend tv-offset-info (offsets posting))
	    ;; FIXME: the ferret code is weird here.
	    #||
	    (when tv-offset-info
	      (setf (aref (offsets posting) freq) tv-offset-info))
	    ||#
	    (setf (freq posting) (+ freq 1)))
	  (let ((term (make-term field text)))
	    (setf (gethash (posting-key term) posting-table)
		  (make-instance 'posting
				 :term term
				 :position position
				 :offset tv-offset-info)))))))

(defgeneric sort-posting-table (document-writer))

(defmethod sort-posting-table ((self document-writer))
  (with-slots (posting-table) self
    (let ((postings (coerce (loop for posting being the hash-values in posting-table
				 collect posting)
			    'vector)))
      (setf postings (sort postings #'term< :key #'term)))))

(defgeneric write-postings (document-writer postings segment))

(defmethod write-postings ((self document-writer) postings segment)
  (with-slots (directory field-infos term-index-interval) self
    (let ((freq nil)
	  (prox nil)
	  (tis-writer nil)
	  (tv-writer nil))
      (unwind-protect
	   (progn
	     (setf freq (open-segment-file directory segment *frq-extension* :output))
	     (setf prox (open-segment-file directory segment *prx-extension* :output))
	     (setf tis-writer (make-instance 'term-infos-writer
					     :directory directory
					     :segment segment
					     :field-infos field-infos
					     :interval term-index-interval))
	       (let ((ti (make-instance 'term-info))
		     (current-field nil))
		 (dosequence (posting postings)
		   (set-values ti 1 (pos freq) (pos prox) -1)
		   (add-term tis-writer (term posting) ti)
		   (let ((posting-freq (freq posting)))
		     (if (= posting-freq 1)
			 (write-vint freq 1)
			 (progn
			   (write-vint freq 0)
			   (write-vint freq posting-freq)))
		     (let ((last-position 0))
		       (dosequence (position (positions posting))
			 (write-vint prox (- position last-position))
			 (setf last-position position)))
		     (let ((term-field (term-field (term posting))))
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
			 (add-term-to-term-vectors-writer tv-writer
							  (term-text (term posting))
							  posting-freq
							  :positions (positions posting)
							  :offsets (offsets posting)))))))
	       (when tv-writer
		 (close-document tv-writer)))
	(progn
	  ;; FIXME raise some exceptions somewhere?
	  (when freq (close freq))
	  (when prox (close prox))
	  (when tis-writer (close tis-writer))
	  (when tv-writer (close tv-writer)))))))

(defgeneric write-norms (document-writer segment))

(defmethod write-norms ((self document-writer) segment)
  (with-slots (directory field-infos field-boosts similarity field-lengths) self
    (dotimes (i (size field-infos))
      (let ((fi (get-field field-infos i)))
	(when (and (field-indexed-p fi) (not (field-omit-norms-p fi)))
	  (let ((norm (* (aref field-boosts i)
			 (length-norm similarity (field-name fi) (aref field-lengths i))))
		(norms (open-segment-file directory segment (format nil "f~S" i) :output)))
	    (unwind-protect
		 (write-byte norms (similarity-encode-norm norm))
	      (close norms))))))))

(defun posting-key (term)
  (cons (term-field term) (term-text term)))

(defclass posting ()
  ((term :initarg :term :reader term)
   (freq :initform 1 :accessor freq)
   (positions :reader positions)
   (offsets :reader offsets)))

(defmethod initialize-instance :after ((self posting) &key position offset)
  (with-slots (positions offsets) self
    (setf positions (make-array 1 :initial-element position
				:adjustable T :fill-pointer T))
    (setf offsets (make-array 1 :initial-element offset
			      :adjustable T :fill-pointer T))))

(defmethod print-object ((self posting) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (term freq positions offsets) self
      (format stream "term: ~S freq: ~S positions: ~S offsets: ~S"
	      term freq positions offsets))))
	    
