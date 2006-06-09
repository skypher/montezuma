(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:require :tbnl)
  (cl:require :cl-who)
  (cl:require :html-template))

(defpackage :pl-search
  (:use :tbnl :common-lisp :html-template))

(in-package :pl-search)

(setq tbnl:*show-lisp-errors-p* T)
(setq tbnl:*show-lisp-backtraces-p* T)
(setq tbnl:*use-apache-log* NIL)

(setq tbnl:*dispatch-table*
      (list
       (tbnl:create-prefix-dispatcher "/index.html" 'main-page)
       (tbnl:create-prefix-dispatcher "/cache.html" 'cache-page)))
       


(defparameter *pl-web-root* (make-pathname :name nil
					   :type nil
					   :defaults *load-pathname*))

(defparameter *main-query-parser* (make-instance 'montezuma::query-parser
						 :default-field "contents"))
(defparameter *test-query-parser* (make-instance 'montezuma::test-query-parser
						 :default-field "contents"))

(defun parse-query (q)
  (montezuma::parse *main-query-parser* q))
(defun query-parse-tree (q)
  (montezuma::parse *test-query-parser* q))

(defun get-field (doc name)
  (montezuma::field-data (montezuma::document-field doc name)))

(defvar *pl-index*
  (make-instance 'montezuma::index
		 :path (make-pathname :name "plindex"
				      :defaults montezuma::*pl-directory*)
		 :create-p NIL
		 :default-field "contents"))


(defun cache-page ()
  (let ((cache-tmpl (merge-pathnames (make-pathname :name "cache"
						    :type "tmpl")
				     *pl-web-root*)))
    (let ((id (tbnl:get-parameter "id")))
      (let ((doc (montezuma::get-document *pl-index* id)))
	(if doc
	    (with-output-to-string (s)
	      (html-template:fill-and-print-template
	       cache-tmpl
	       `(:title ,(get-field doc "title")
		 :description ,(get-field doc "description"))
	       :stream s))
	    (error "No document with id ~S seems to exist." id))))))

(defun main-page ()
  (let ((main-tmpl (merge-pathnames (make-pathname :name "index"
						   :type "tmpl")
				    *pl-web-root*)))
    (let* ((query-string (tbnl:get-parameter "q"))
	   (escaped-query-string (cl-who:escape-string query-string))
	   (query-parse-tree nil)
	   (query-object nil)
	   (results nil)
	   (num-results nil)
	   (query-time nil))
      (when query-string
	(setf query-string (string-trim '(#\space #\tab) query-string))
	(setf query-parse-tree
	      (let ((tree (query-parse-tree query-string)))
		(if tree
		    (format nil "~%<pre>~A</pre>~%"
			    (cl-who:escape-string
			     (with-output-to-string (s)
			       (pprint tree s))))
		    (format nil "<p class=\"error\">Your query, &ldquo;~A&rdquo;, could not be parsed.  Sorry!</p>"
			    escaped-query-string))))
	(setf query-object
	      (let ((query (parse-query query-string)))
		(if (not query)
		    nil
		    (format nil "~%<pre>~A</pre>~%"
			    (cl-who:escape-string
			     (with-output-to-string (s)
			       (pprint query s)))))))
	(let ((query (parse-query query-string)))
	  (when query
	    (let ((start-time (get-internal-real-time)))
	      (montezuma::search-each
	       *pl-index* query
	       #'(lambda (doc score)
		   (let ((document (montezuma::get-document *pl-index* doc)))
		     (push (list :score (format nil "~,3F" score)
				 :date (get-field document "date")
				 :title (let ((title (get-field document "title")))
					  (if (= (length title) 0)
					      "[no title]"
					      title))
				 :id (tbnl:url-encode (get-field document "id"))
				 :link (cl-who:escape-string (get-field document "link")))
			   results)))
	       '(:num-docs 50))
	      (setf query-time (format nil "~,3F" (/ (- (get-internal-real-time) start-time)
						    internal-time-units-per-second))))
	    (setf num-results (format nil "~A" (length results)))
	    (setf results (reverse results)))))
      (format T "~&Results: ~S" results)
      (with-output-to-string (s)
	(html-template:fill-and-print-template
	 main-tmpl
	 `(:query-string ,escaped-query-string
	   :query-object ,query-object
	   :query-parse-tree ,query-parse-tree
	   :num-results ,num-results
	   :query-time ,query-time
	   :index-size ,(format nil "~A" (montezuma::size *pl-index*))
	   :results ,results)
	 :stream s)))))

