(in-package #:montezuma)

(defparameter *index-writer-write-lock-timeout* 1)
(defparameter *index-writer-commit-lock-timeout* 10)
(defparameter *index-writer-write-lock-name* "write.lock")
(defparameter *index-writer-commit-lock-name* "commit.lock")
(defparameter *index-writer-default-merge-factor* 10)
(defparameter *index-writer-default-min-merge-docs* 10)
(defparameter *index-writer-default-max-merge-docs* #x7fffffff)
(defparameter *index-writer-default-max-field-length* 1000)
(defparameter *index-writer-default-term-index-interval* 128)

(defclass index-writer ()
  ((directory :initarg :directory)
   (close-dir-p :initarg :close-dir-p)
   (use-compound-file-p :initarg :use-compound-file-p)
   (analyzer :initarg :analyzer)
   (merge-factor :initarg :merge-factor)
   (min-merge-docs :initarg :min-merge-docs)
   (max-merge-docs :initarg :max-merge-docs)
   (max-field-length :initarg :max-field-length)
   (term-index-interval :initarg :term-index-interval)
   (similarity :initform (make-default-similarity))
   (segment-infos :initform (make-instance 'segment-infos))
   ;; FIXME write-lock
   (ram-directory :initform (make-instance 'ram-directory))
   (info-stream :initform nil))
  (:default-initargs
   :close-dir-p          NIL
    :use-compound-file-p T
    :analyzer            (make-instance 'standard-analyzer)
    :merge-factor        *index-writer-default-merge-factor*
    :min-merge-docs      *index-writer-default-min-merge-docs*
    :max-merge-docs      *index-writer-default-max-merge-docs*
    :max-field-length    *index-writer-default-max-field-length*
    :term-index-interval *index-writer-default-term-index-interval*))

(defmethod initialize-instance :after ((self index-writer) &key (create-p NIL))
  (with-slots (directory segment-infos) self
    (cond ((null directory) (setf directory (make-instance 'ram-directory)))
	  ((stringp directory) (setf directory (make-fs-directory directory :create-p T))))
    (if create-p
	(write-segment-infos segment-infos directory)
	(progn
	  (read-segment-infos segment-infos directory)
	  ;; FIXME: handle missing segment infos and :create-if-missing-p
	  ))))

(defmethod close ((self index-writer))
  (flush-ram-segments self)
  (with-slots (ram-directory close-dir-p directory) self
    (close ram-directory)
    (when close-dir-p
      (close directory))))

(defmethod document-count ((self index-writer))
  (with-slots (segment-infos) self
    (let ((count 0))
      (dotimes (i (size segment-infos) count)
	(incf count (doc-count (segment-info segment-infos i)))))))

(defmethod add-document-to-index-writer ((self index-writer) document &optional (analyzer nil analyzer-supplied-p))
  (with-slots (ram-directory similarity max-field-length term-index-interval info-stream
			     segment-infos) self
    (unless analyzer-supplied-p
      (setf analyzer (slot-value self 'analyzer)))
    (let ((dw (make-instance 'document-writer
			     :directory ram-directory
			     :analyzer analyzer
			     :similarity similarity
			     :max-field-length max-field-length
			     :term-index-interval term-index-interval))
	  (segment-name (new-segment-name self)))
      (setf (info-stream dw) info-stream)
      (add-document-to-writer dw segment-name document)
      ;; FIXME synchronize
      (add-segment-info segment-infos
			(make-instance 'segment-info
				       :name segment-name
				       :doc-count 1
				       :directory ram-directory))
      (maybe-merge-segments self))))


(defmethod optimize ((self index-writer))
  (flush-ram-segments self)
  (with-slots (segment-infos merge-factor directory use-compound-file-p) self
    ;; FIXME: Insane.
    (while (or (> (size segment-infos) 1)
	       (and (= (size segment-infos) 1)
		    (let ((si (segment-info segment-infos 0)))
		      (or (has-deletions-p si)
			  (or (not (eq directory (directory si)))
			      (and use-compound-file-p
				   (or (uses-compound-file-p si)
				       (has-separate-norms-p si))))))))
	(let ((min-segment (- (size segment-infos) merge-factor)))
	  (merge-segments self (max 0 min-segment))))))

(defmethod add-indexes ((self index-writer) dirs)
  (optimize self)
  (with-slots (segment-infos merge-factor) self
    (let ((start (size segment-infos)))
      (dolist (dir dirs)
	(let ((sis (make-instance 'segment-infos)))
	  (read-segment-infos sis dir)
	  (dotimes (i (size sis))
	    (add-segment-info segment-infos (segment-info sis i)))))
      (while (> (size segment-infos) (+ start merge-factor))
	(do-range (base (+ start 1) (size segment-infos))
	  (let ((last (min (size segment-infos) (+ base merge-factor))))
	    (when (> (- last base) 1)
	      (merge-segments self base last)))))
      (optimize self))))

(defmethod add-indexes-readers ((self index-writer) readers)
  (let ((segments-to-delete '())
	(merged-name (new-segment-name self)))
    (optimize self)
    (with-slots (directory term-index-interval segment-infos) self
    (let ((merge (make-instance 'segment-merger
				:directory directory
				:name merged-name
				:term-index-interval term-index-interval)))
      (when (= (size segment-infos) 1)
	(let ((s-reader (get-segment-reader (segment-info segment-infos 0))))
	  (add-segment merger s-reader)
	  (push segments-to-delete s-reader)))
      (dolist (reader readers)
	(add-segment merger reader))
      (let ((doc-count (merge merger)))
	(clear segment-infos)
	(add-segment-info segment-infos (make-instance 'segment-info
						       :segment merged-name
						       :doc-count doc-count
						       :directory directory))
	(write segment-infos directory)
	(delete-segments self segments-to-delete)
	(when use-compound-file-p
	  (let ((files-to-delete (create-compound-file merger (format nil "~A.tmp" merged-name))))
	    (rename-file directory
			 (add-file-extension merged-name "tmp")
			 (add-file-extension merged-name "cfs"))
	    (delete-files-and-write-undeletable self files-to-delete))))))))

(defmethod new-segment-name ((self index-writer))
  (with-slots (segment-infos) self
    (let ((seg-name (format nil "_~36R" (counter segment-infos))))
      (incf (counter segment-infos))
      seg-name)))

(defmethod flush-ram-segments ((self index-writer))
  (with-slots (segment-infos ram-directory merge-factor) self
    (let ((min-segment (- (size segment-infos) 1))
	  (doc-count 0))
      (while (and (>= min-segment 0)
		  (directory= (directory (segment-info segment-infos min-segment)) ram-directory))
	(incf doc-count (doc-count (segment-info segment-infos min-segment)))
	(decf min-segment))
      (when (or (< min-segment 0)
		(> (+ doc-count (doc-count (segment-info segment-infos min-segment))) merge-factor)
		(not (directory= (directory (segment-info segment-infos (- (size segment-infos) 1)))
				 ram-directory)))
	(incf min-segment))
      (when (< min-segment (size segment-infos))
	(merge-segments self min-segment)))))

(defmethod maybe-merge-segments ((self index-writer))
  (with-slots (min-merge-docs max-merge-docs segment-infos) self
    (let ((target-merge-docs min-merge-docs))
      (while (<= target-merge-docs max-merge-docs)
	(let ((min-segment (- (size segment-infos) 1))
	      (merge-docs 0))
	  (while (>= min-segment 0)
	    (let ((si (segment-info segment-infos min-segment)))
	      (when (>= (doc-count si) target-merge-docs)
		(return))
	      (incf merge-docs (doc-count si))
	      (decf min-segment)))
	  (if (>= merge-docs target-merge-docs)
	      (merge-segments self (+ min-segment 1))
	      (return))
	  (setf target-merge-docs (* target-merge-docs merge-factor)))))))

(defmethod merge-segments ((self index-writer) min-segment &optional (max-segment nil max-segment-supplied-p))
  (with-slots (segment-infos info-stream term-index-interval directory) self
    (unless max-segment-supplied-p
      (setf max-segment (size segment-infos)))
    (let ((segments-to-delete '())
	  (merged-name (new-segment-name self)))
      (when info-stream
	(format info-stream "~&Merging segments from ~S to ~S"
		min-segment (- max-segment 1)))
      (let ((merger (make-instance 'segment-merger
				   :directory directory
				   :name merged-name
				   :term-index-interval term-index-interval)))
	(do-range (i min-segment max-segment)
	  (let ((si (segment-info segment-infos i)))
	    (when info-stream
	      (format info-stream "~&~S (~S docs)"
		      (name si)
		      (doc-count si)))
	    (let ((reader (make-instance 'segment-reader
					 :directory (directory si)
					 :info si
					 :segment-infos nil
					 :close-directory-p NIL
					 :directory-owner NIL)))
	      (add-segment-reader merger reader)
	      (when (or (directory= (directory reader) directory)
			(directory= (directory reader) ram-directory))
		(push reader segments-to-delete)))))
	(let ((merged-doc-count (merge merger)))
	  (when info-stream
	    (format info-stream "~& into ~S (~S docs)"
		    merged-name merged-doc-count ))
	  (loop for i from (- max-segment 1) downto min-segment
	       do (delete-at segment-infos i))
	  (add-segment-info segment-infos (make-instance 'segment-info
							 :segment merged-name
							 :count merged-doc-count
							 :directory directory))
	  (close-readers merger)
	  (write-segment-infos segment-infos directory)
	  (delete-segments segments-to-delete)
	  (when use-compound-file-p
	    (let ((files-to-delete (create-compound-file merger (add-extension merged-name "tmp"))))
	      (rename-file directory
			   (add-extension merged-name "tmp")
			   (add-extension merged-name "cfs"))
	      (delete-files-and-write-undeletable self files-to-delete))))))))

(defmethod delete-segments ((self index-writer) segment-readers)
  (with-slots (directory) self
    (let ((deletable (try-to-delete-files self (read-deletable-files self))))
      (dolist (segment-reader segment-readers)
	(if (directory= (directory segment-reader) directory)
	    (setf deletable (append deletable (try-to-delete-files self (file-names segment-reader))))
	    (delete-files (file-names segment-reader (directory segment-reader)))))
      (write-deletable-files deletable))))

(defmethod delete-files-and-write-undeletable ((self index-writer) files)
  (let ((deletable (append (try-to-delete-files self (read-deletable-files self))
			   (try-to-delete-files self files deletable))))
    (write-deletable-files deletable)))

(defmethod delete-files ((self index-writer) filenames dir)
  (dolist (filename filenames)
    (delete-file dir filename)))

(defmethod try-to-delete-files ((self index-writer) filenames)
  (with-slots (directory) self
    (let ((deletions-to-retry '()))
      (dolist (filename filenames)
	;; FIXME: when can't you delete a file?
	(delete-file directory filename))
      deletions-to-retry)))
    
(defmethod read-deletable-files ((self index-writer))
  (with-slots (directory) self
    (if (not (file-exists-p directory "deletable"))
	'()
	(let ((filenames '())
	      (input (open-input directory "deletable")))
	  (unwind-protect
	       (let ((file-count (read-int input)))
		 (dotimes (i file-count)
		   (push (read-string input) filenames)))
	    (close input))
	  filenames))))

(defmethod write-deletable-files ((self index-writer) filenames)
  (with-slots (directory) self
    (let ((output (create-output directory "deletable.new")))
      (unwind-protect
	   (progn
	     (write-int output (length filenames))
	     (dolist (filename filenames)
	       (write-string output filename)))
	(close output)))
    (rename-file directory "deletable.new" "deletable")))
