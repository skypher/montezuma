(cl:defpackage #:planet-lisp-search
  (:use #:common-lisp)
  (:export #:*posts-path*
	   #:*index-path*
	   #:index-posts
	   #:search-posts))

(in-package #:planet-lisp-search)


;; -- Variables

(defvar *posts-path*
  (merge-pathnames (make-pathname :directory '(:relative "posts"))
		   (make-pathname :name nil
				  :type nil
				  :defaults *load-pathname*))
  "The directory containing the Planet Lisp archives.")


(defvar *index-path*
  (merge-pathnames (make-pathname :directory '(:relative "plindex"))
		   (make-pathname :name nil
				  :type nil
				  :defaults *load-pathname*))
  "The path at which to store the index.")

(defvar *index* nil
  "The Montezuma index for the Planet Lisp archives.")


;; -- Macros

(defmacro time-form (format form)
  `(multiple-value-bind (result time)
       (time-thunk #'(lambda () ,form))
     (format T ,format time)
     result))

(defun time-thunk (thunk)
  (let* ((start (get-internal-run-time))
	 (result (funcall thunk))
	 (end (get-internal-run-time)))
    (values result (/ (- end start) internal-time-units-per-second))))


;; -- Indexing

(defun index-posts ()
  "Builds an index of all Planet Lisp posts from scratch."
  (let ((index (make-instance 'montezuma:index
			      :path *index-path*
			      :create-p T
			      ;; Setting :min-merge-docs to 5000 helps keep most
			      ;; of the indexing in RAM.  You might want to
			      ;; decrease this if you're RAM-starved.
			      :min-merge-docs 5000))
	(files (all-post-files))
	(ids (make-hash-table :test #'equal)))
    (format T "~&Indexing ~S posts... " (length files))
    (time-form
     "Indexing took ~,3F seconds."
     (dolist (file files)
       (let* ((post (read-post-file file))
	      (id (getf post :id)))
	 ;; The Planet Lisp archives seem to have some duplicates.
	 (if (gethash id ids)
	     (warn "Skipping post with duplicate ID ~S" id)
	     (progn
	       (setf (gethash id ids) T)
	       (index-post index post))))))
    (format T "~&Indexed ~S unique posts." (hash-table-count ids))
    (format T "~&Optimizing... ")
    ;; Once all the posts have been added to the index, optimize it for queries.
    (time-form "Optimizing took ~,3F seconds."
	       (montezuma:optimize index))
    (montezuma:close index)
    (values)))

						   
(defun index-post (index post)
  "Adds a single Planet List post to the index."
  (destructuring-bind (&key md5 id title description date link)
      post
    ;; Each post is a separate document.
    (let ((doc (make-instance 'montezuma:document)))
      ;; We want to be able to search on the MD5, ID and LINK fields and
      ;; retrieve them from the index, but they should not be tokenized.  (MD5
      ;; is the md5 hash of the post, which is actually taken from the archive
      ;; filename.)
      (montezuma:add-field doc (montezuma:make-field "md5" md5 :index :untokenized))
      (montezuma:add-field doc (montezuma:make-field "id" id :index :untokenized))
      (montezuma:add-field doc (montezuma:make-field "link" link :index :untokenized))
      ;; The TITLE should be tokenized.  And while it's not super clear in RSS
      ;; at least, titles are often HTML.
      (montezuma:add-field doc (montezuma:make-field "title" (strip-html title) :index :tokenized))
      ;; Convert dates from universal times to strings like "2006-06-06".
      ;; Because of the way Montezuma does DATE queries (the secret is that
      ;; they're no different from any other query), there is some advantage in
      ;; decreasing the number of unique terms, so we limit the resolution to
      ;; one day.
      (montezuma:add-field doc (montezuma:make-field "date" (date-string date) :index :untokenized))
      ;; The DATEDISPLAY field is not meant to be queried, it exists only to
      ;; show search results with timestamps of a nice 1 second resolution, like
      ;; "2006-06-06 23:59:59".
      (montezuma:add-field doc (montezuma:make-field "displaydate" (timestamp-string date) :index NIL))
      ;; The DESCRIPTION (the HTML contents of the post) should be tokenized.  I
      ;; also store it in the index so that the web search interface can offer
      ;; "Cached" links that display the entire post.  If Planet Lisp kept posts
      ;; in a database, say, the cached version could be retrieved from there
      ;; instead of from the Montezuma index.
      (montezuma:add-field doc (montezuma:make-field "description" description :index :tokenized))
      ;; To avoid the problem of, e.g, a phrase query like "OS X sucks" not
      ;; matching HTML like "OS X <b>sucks</b>", we index a TEXT-only version of
      ;; the post.  We don't need to store a copy of this field.
      (montezuma:add-field doc (montezuma:make-field "text" (strip-html description)
						     :index :tokenized :stored NIL))
      ;; Finally, add the document to the index.
      (montezuma:add-document-to-index index doc))))
  

(defun all-post-files ()
  "Returns a list of all .sexp post archive files."
  (let ((files '()))
    (cl-fad:walk-directory *posts-path*
			   #'(lambda (path)
			       (when (string= (pathname-type path) "sexp")
				 (push path files))))
    files))


(defun read-post-file (file)
  "Reads a single post file."
  (with-open-file (in file :direction :input
		      ;; Ugh. Lisp makes me angry sometimes.  What am I supposed
		      ;; to do, require flexi-streams?  Anyway, this is not
		      ;; portable.
		      :external-format :latin-1)
    ;; Archive files just contain a list, so use COMMON-LISP:READ to read it.
    (with-standard-io-syntax
      (let ((post (cl:read in)))
	  ;; Add an :md5 property (taken from the filename,
	  ;; e.g. "a4cadb7008415fe24a6b0585523900dd.sexp") just to keep track of
	  ;; it.
	(setf post (list* :md5 (pathname-name file) (cdr post)))
	post))))


;; -- Searching

(defun load-index ()
  (setf *index* (make-instance 'montezuma:index
			       :path *index-path*
			       :create-p NIL
			       :create-if-missing-p NIL
			       ;; Unless otherwise specified, queries will
			       ;; search all these fields simultaneously.
			       :default-field "*"
			       :fields '("title" "description" "text"))))

;; Example searches:
;;
;; (search-posts "uav")
;; (search-posts "+title:robot embedded lisp")
;; (search-posts "opeth" '(:num-docs 30))
;; (search-posts "bug*" '(:num-docs 10 :start-doc 10))
;; (search-posts "bug*" '(:num-docs 10000) T)
;; (search-posts "date:2005-12*")

(defun search-posts (query &optional options count-only-p)
  (unless *index*
    (load-index))
  (let ((results '()))
    (multiple-value-bind (value time)
	(time-thunk #'(lambda ()
			(montezuma:search-each *index* query
					       #'(lambda (doc score)
						   (push (cons doc score) results))
					       options)))
      (declare (ignore value))
      (unless count-only-p
	(dolist (result (reverse results))
	  (print-result (car result) (cdr result))))
      (format T "~&~S results in ~,3F seconds." (length results) time))))


(defun print-result (doc score)
  (let ((paste (montezuma:get-document *index* doc)))
    (flet ((get-field (name) 
	     (montezuma:field-data (montezuma:document-field paste name))))
      (let ((title (get-field "title")))
	(when (= (length title) 0)
	  (setf title "[untitled]"))
	(format T "~&~5,2F - ~A~&  ~A~&  link: ~A~&  id: ~A~&~%"
		score
		title
		(get-field "displaydate")
		(get-field "link")
		(get-field "id"))))))


;; -- Misc.

(defun strip-html (string)
  "Removes HTML tags from a string.  e.g., \"foo <b>bar</b>\" becomes \"foo bar\"."
  ;; This should really handle character entities, too.
  (let ((in-tag-p NIL))
    (with-output-to-string (s)
      (dotimes (i (length string))
	(let ((char (char string i)))
	  (cond ((and (not in-tag-p) (not (eql char #\<)))
		 ;; Text
		 (write-char char s))
		((and (not in-tag-p) (eql char #\<))
		 ;; Open tag
		 (setf in-tag-p T))
		((and in-tag-p (not (eql char #\>)))
		 ;; Tag text
		 )
		((and in-tag-p (eql char #\>))
		 ;; Close tag
		 (setf in-tag-p NIL))))))))


(defun date-string (universal-time)
  "Turns a universal time into a string like \"2006-06-06\"."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~D-~2,'0D-~2,'0D" year month date)))


(defun timestamp-string (universal-time)
  "Turns a universal time into a string like \"2006-06-06 23:59:59\"."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    year month date hour minute second)))
