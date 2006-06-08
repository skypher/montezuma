(in-package #:montezuma)


(defun time-thunk (thunk)
  (let* ((start (get-internal-run-time))
	 (result (funcall thunk))
	 (end (get-internal-run-time)))
    (values result (/ (- end start) internal-time-units-per-second))))

(defmacro time-form (format form)
  `(multiple-value-bind (result time)
    (time-thunk #'(lambda () ,form))
    (format T ,format time)
    result))


(defun index-post (index post)
  (destructuring-bind (&key md5 id title description date link)
      post
    (let ((doc (make-instance 'document)))
      (add-field doc (make-field "md5" md5 :index :untokenized :stored T))
      (add-field doc (make-field "id" id :index :untokenized :stored T))
      (add-field doc (make-field "title" title :index :tokenized :stored T))
      (add-field doc (make-field "description" description :index :tokenized :stored T))
      (add-field doc (make-field "contents"
				 (format nil "~A ~A" title description)
				 :index :tokenized :stored NIL))
      (add-field doc (make-field "date" (date-string date) :index :untokenized :stored T))
      (add-field doc (make-field "link" link :index :untokenized :stored T))
      (add-document-to-index index doc))))

(defvar *pl-directory* (make-pathname :name nil
				      :type nil
				      :defaults *load-pathname*))



(defun post-files ()
  (let ((files '()))
    (cl-fad:walk-directory
     (merge-pathnames (make-pathname :directory '(:relative "posts"))
		      *pl-directory*)
     #'(lambda (path)
	 (when (string= (pathname-type path) "sexp")
	   (push path files))))
    files))

(defun index-post-file (index file)
  (with-open-file (in file :direction :input :external-format :latin-1)
    (let ((post (cl:read in)))
      (setf post (list* :md5 (pathname-name file) (cdr post)))
      (index-post index post))))


(defun date-string (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    year month date hour minute second)))

		 

(defun index-posts ()
  (let ((index (make-instance 'index
			      :path (make-pathname :name "plindex"
						   :defaults *pl-directory*)
			      :create-p T
			      :default-field "contents"
			      :min-merge-docs 5000)))
    (let ((files (post-files)))
      (format T "~&Indexing ~S posts... " (length files))
      (time-form "Indexing took ~,3F seconds."
		 (dolist (file files)
		   (index-post-file index file)))
      (format T "~&Optimizing... ")
      (time-form "Optimizing took ~,3F seconds."
		 (optimize index)))
    (close index)
    index))

						   
(defun search-posts (index field query &optional options)
  (etypecase query
    (list
     ;; Make a boolean query where each clause is a wildcard query
     ;; that MUST occur.
     (let ((words query))
       (setf query (make-instance 'boolean-query))
       (dolist (word words)
	 (add-query query
		    (make-instance 'wildcard-query
				   :term (make-term field word))
		    :must-occur))))
    (string
     ;; Make a single-term wildcard query.
     (let ((word query))
       (setf query (make-instance 'wildcard-query
				  :term (make-term field word)))))
    (query
     ;; Don't need to do anything, use it as-is.
     ))
  ;; Perform the search
  (let ((num-results 0))
    (search-each index query
		 #'(lambda (doc score)
		     (when (= num-results 0)
		       (format T "~&~5A ~19A ~5A ~15A" "Score" "Date" "#" "User")
		       (format T "~&-------------------------------------------"))
		     (incf num-results)
		     (print-result index doc score))
		 options)
    (format T "~&~%~S results displayed." num-results)))

(defun print-result (index doc score)
  (let ((paste (get-document index doc)))
    (format T "~&~5,2F ~A ~A ~15A~&~vt~A"
	    score
	    (field-data (document-field paste "date"))
	    (field-data (document-field paste "md5"))
	    (field-data (document-field paste "id"))
	    10
	    (field-data (document-field paste "title")))))

