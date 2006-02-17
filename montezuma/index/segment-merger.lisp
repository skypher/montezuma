(in-package #:montezuma)

(defclass segment-merger ()
  ((directory :initarg :directory)
   (segment :initarg :name)
   (term-index-interval :initarg :term-index-interval)
   (readers :initform (make-array 0 :adjustable T :fill-pointer T))
   (field-infos :initform '())
   (freq-output :initform nil)
   (prox-output :initform nil)
   (term-infos-writer :initform nil)
   (queue :initform nil)
   (term-info :initform (make-instance 'term-info))
   (skip-buffer
    :initform (make-instance 'ram-index-output
			     :file (make-instance 'ram-file :name "")))
   (skip-interval)
   (last-skip-doc)
   (last-skip-freq-pointer)
   (last-skip-prox-pointer))
  (:default-initargs
   :term-index-interval *index-writer-default-term-index-interval*))


(defmethod add-reader ((self segment-merger) reader)
  (with-slots (readers) self
    (vector-push-extend reader readers)))

(defmethod segment-reader ((self segment-merger) i)
  (with-slots (readers) self
    (aref readers i)))

(defmethod merge ((self segment-merger))
  (with-slots (field-infos) self
    (let ((value (merge-fields self)))
      (merge-terms self)
      (merge-norms self)
      (when (has-vectors-p field-infos)
	(merge-vectors))
    value)))

(defmethod close-readers ((self segment-merger))
  (dosequence (reader (slot-value self 'readers))
    (close reader)))

(defmethod create-compound-file ((self segment-merger) filename)
  (with-slots (directory segment field-infos) self
    (let ((cfs-writer (make-instance 'compound-file-writer
				     :directory directory
				     :file-name filename))
	  (files '()))
      (dolist (extension *index-compound-extensions*)
	(push (add-file-extension segment extension) files))
      (dotimes (i (size field-infos))
	(let ((fi (field-info field-infos i)))
	  (when (and (field-indexed-p fi) (not (field-omit-norms-p fi)))
	    (push (add-file-extension segment (format nil "f~D" i)) files))))
      (when (has-vectors-p field-infos)
	(dolist (extension *index-vector-extensions*)
	  (push (add-file-extension segment extension) files)))
      (dolist (file files)
	(add-file cfs-writer file))
      (close cfs-writer)
      files)))


(defmethod add-indexed ((self segment-merger) reader field-infos field-names store-term-vectors
			store-position-with-term-vector store-offset-with-term-vector)
  (dosequence (field field-names)
    (add-field-info field-infos
		    field
		    :indexed-p T
		    :store-term-vector store-term-vectors
		    :store-position store-position-with-term-vector
		    :store-offset store-offset-with-term-vector
		    :omit-norms (not (has-norms-p reader field)))))


(defmethod merge-fields ((self segment-merger))
  (with-slots (field-infos readers segment directory) self
    (setf field-infos (make-instance 'field-infos))
    (let ((doc-count 0))
      (dosequence (reader readers)
	(add-indexed self reader field-infos
		     (get-field-names reader :term-vector-with-position-offset) T T T)
	(add-indexed self reader field-infos
		     (get-field-names reader :term-vector-with-position) T T NIL)
	(add-indexed self reader field-infos
		     (get-field-names reader :term-vector-with-offset) T NIL T)
	(add-indexed self reader field-infos
		     (get-field-names reader :term-vector) T NIL NIL)
	(add-indexed self reader field-infos
		     (get-field-names reader :indexed) NIL NIL NIL)
	(add-fields field-infos (get-field-names reader :unindexed) :indexed-p NIL))
      (write-to-dir field-infos directory (add-file-extension segment "fnm"))
      (let ((fields-writer (make-instance 'fields-writer
					  :directory directory
					  :segment segment
					  :field-infos field-infos)))
	(unwind-protect
	     (dosequence (reader readers)
	       (let ((max-doc (max-doc reader)))
		 (dotimes (j max-doc)
		   (unless (deleted-p reader j)
		     (add-document fields-writer (get-document reader j))
		     (incf doc-count)))))
	  (close fields-writer))
	doc-count))))

(defmethod merge-vectors ((self segment-merger))
  (with-slots (directory segment readers field-infos) self
    (let ((term-vectors-writer (make-instance 'term-vectors-writer
					      :directory directory
					      :segment segment
					      :field-infos field-infos)))
      (unwind-protect
	   (dosequence (reader readers)
	     (let ((max-doc (max-doc reader)))
	       (dotimes (doc-num max-doc)
		 (unless (deleted-p reader doc-num)
		   (add-all-doc-vectors term-vectors-writer (get-term-vectors reader doc-num))))))
	(close term-vectors-writer)))))

(defmethod merge-terms ((self segment-merger))
  (with-slots (directory segment readers freq-output prox-output term-infos-writer
	       skip-interval queue field-infos term-index-interval) self
    (unwind-protect
	 (progn
	   (setf freq-output (create-output directory (add-file-extension segment "frq"))
		 prox-output (create-output directory (add-file-extension segment "prx"))
		 term-infos-writer (make-instance 'term-infos-writer
						  :directory directory
						  :segment segment
						  :field-infos field-infos
						  :interval term-index-interval))
	   (setf skip-interval (skip-interval term-infos-writer)
		 queue (make-instance 'segment-merge-queue :max-size (length readers)))
	   (merge-term-infos self))
      (print (list freq-output prox-output term-infos-writer queue))
      (when freq-output (close freq-output))
      (when prox-output (close prox-output))
      (when term-infos-writer (close term-infos-writer))
      (when queue (close queue)))))

(defmethod merge-term-infos ((self segment-merger))
  (with-slots (readers queue) self
    (let ((base 0))
      (dosequence (reader readers)
	(let* ((term-enum (terms reader))
	       (smi (make-instance 'segment-merge-info
				   :base base
				   :term-enum term-enum
				   :reader reader)))
	  (incf base (num-docs reader))
	  (if (next smi)
	      (queue-push queue smi)
	      (close smi))))
      (let ((match (make-array (length readers))))
	(while (> (size queue) 0)
	  (let ((match-size 0))
	    (setf (aref match match-size) (queue-pop queue))
	    (incf match-size)
	    (let ((term-buffer (term-buffer (aref match 0)))
		  (top (queue-top queue)))
	      (while (and (not (null top))
			  (term-buffer= (term-buffer top) term-buffer))
		(setf (aref match match-size) (queue-pop queue))
		(incf match-size)
		(setf top (queue-top queue)))
	      (merge-term-info self match match-size)
	      (while (> match-size 0)
		(decf match-size)
		(let ((smi (aref match match-size)))
		  (if (next smi)
		      (queue-push queue smi)
		      (close smi)))))))))))

(defmethod merge-term-info ((self segment-merger) smis n)
  (with-slots (freq-output prox-output term-info term-infos-writer) self
    (let ((freq-pointer (pos freq-output))
	  (prox-pointer (pos prox-output))
	  (df (append-postings self smis n))
	  (skip-pointer (write-skip self)))
      (when (> df 0)
	(set-values term-info df freq-pointer prox-pointer (- skip-pointer freq-pointer))
	(add-term term-infos-writer (term (term-buffer (aref smis 0))) term-info)))))

(defmethod append-postings ((self segment-merger) smis n)
  (with-slots (freq-output prox-output skip-interval) self
    (let ((last-doc 0)
	  (df 0))
      (reset-skip self)
      (dotimes (i n)
	(let* ((smi (aref smis i))
	       (postings (positions smi))
	       (base (base smi))
	       (doc-map (doc-map smi)))
	  (seek postings (term-enum smi))
	  (while (next postings)
	    (let ((doc (doc postings)))
	      (when (not (null doc-map))
		(setf doc (aref doc-map doc)))
	      (incf doc base)
	      (when (< doc last-doc)
		(error "Docs out of order; current doc is ~S and previous doc is ~S" doc last-doc))
	      (incf df)
	      (when (= (mod df skip-interval) 0)
		(buffer-skip self last-doc))
	      (let ((doc-code (ash (- doc last-doc) 1)))
		(setf last-doc doc)
		(let ((freq (freq postings)))
		  (if (= freq 1)
		      (write-vint freq-output (logior doc-code 1))
		      (progn
			(write-vint freq-output doc-code)
			(write-vint freq-output freq)))
		  (let ((last-position 0))
		    (dotimes (j freq)
		      (let ((position (next-position postings)))
			(write-vint prox-output (- position last-position))
			(setf last-position position))))))))))
      df)))

(defmethod reset-skip ((self segment-merger))
  (with-slots (skip-buffer last-skip-doc last-skip-freq-pointer
	       last-skip-prox-pointer freq-output prox-output
	       skip-buffer) self
    (reset skip-buffer)
    (setf last-skip-doc 0)
    (setf last-skip-freq-pointer (pos freq-output))
    (setf last-skip-prox-pointer (pos prox-output))))

(defmethod buffer-skip ((self segment-merger) doc)
  (with-slots (skip-buffer freq-output prox-output last-skip-prox-pointer
	       last-skip-freq-pointer last-skip-doc) self
    (let ((freq-pointer (pos freq-output))
	  (prox-pointer (pos prox-output)))
      (write-vint skip-buffer (- doc last-skip-doc))
      (write-vint skip-buffer (- freq-pointer last-skip-freq-pointer))
      (write-vint skip-buffer (- prox-pointer last-skip-prox-pointer))
      (setf last-skip-doc doc
	    last-skip-freq-pointer freq-pointer
	    last-skip-prox-pointer prox-pointer))))

(defmethod write-skip ((self segment-merger))
  (with-slots (freq-output skip-buffer) self
    (let ((skip-pointer (pos freq-output)))
      (write-to skip-buffer freq-output)
      skip-pointer)))

(defmethod merge-norms ((self segment-merger))
  (with-slots (field-infos readers segment directory) self
    (dotimes (i (size field-infos))
      (let ((fi (get-field field-infos i)))
	(when (and (field-indexed-p fi)
		   (not (field-omit-norms-p fi)))
	  (let ((output (open-segment-file directory segment (format nil "f~S" i) :output)))
	    (unwind-protect
		 (dosequence (reader readers)
		   (let* ((max-doc (max-doc reader))
			  (input (make-array max-doc)))
		     (get-norms-into reader (field-name fi) input 0)
		     (dotimes (k max-doc)
		       (unless (deleted-p reader k)
			 (write-byte output (aref input k))))))
	      (close output))))))))
