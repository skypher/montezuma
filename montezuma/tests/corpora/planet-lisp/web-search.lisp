(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :tbnl)
  (require :cl-who)
  (require :html-template))

(in-package #:planet-lisp-search)

(setq tbnl:*show-lisp-errors-p* T)
(setq tbnl:*show-lisp-backtraces-p* T)
(setq tbnl:*use-apache-log* NIL)

(setq tbnl:*dispatch-table*
      (list
       (tbnl:create-prefix-dispatcher "/" 'main-page)
       (tbnl:create-prefix-dispatcher "/cache.html" 'cache-page)))
       


(defparameter *pl-web-root* (make-pathname :name nil
					   :type nil
					   :defaults *load-pathname*))

(defparameter *main-query-parser* (make-instance 'montezuma::query-parser
						 :fields '("title" "description" "text")))


(defun parse-query (q)
  (montezuma::parse *main-query-parser* q))


(defun get-field (doc name)
  (montezuma::field-data (montezuma::document-field doc name)))


(defun cache-page ()
  (unless *index*
    (load-index))
  (let ((cache-tmpl (merge-pathnames (make-pathname :name "cache"
						    :type "tmpl")
				     *pl-web-root*)))
    (let ((id (tbnl:get-parameter "id")))
      (let ((doc (montezuma::get-document *index* id)))
	(if doc
	    (with-output-to-string (s)
	      (html-template:fill-and-print-template
	       cache-tmpl
	       `(:title ,(get-field doc "title")
		 :description ,(get-field doc "description"))
	       :stream s))
	    (error "No document with id ~S seems to exist." id))))))


(defun main-page ()
  (unless *index*
    (load-index))
  (let ((main-tmpl (merge-pathnames (make-pathname :name "index"
						   :type "tmpl")
				    *pl-web-root*)))
    (let* ((query-string (tbnl:get-parameter "q"))
	   (start-doc (parse-integer (or (tbnl:get-parameter "start") "0")))
	   (escaped-query-string (cl-who:escape-string query-string))
	   (query-object nil)
	   (results nil)
	   (num-results nil)
	   (query-time nil))
      (when query-string
	(setf query-string (string-trim '(#\space #\tab) query-string))
	(setf query-object
	      (let ((query (parse-query query-string)))
		(format nil "~%<pre>~A</pre>~%"
			(cl-who:escape-string
			 (with-output-to-string (s)
			   (pprint query s))))))
	(let ((query (parse-query query-string)))
	  (when query
	    (let ((start-time (get-internal-real-time)))
	      (montezuma::search-each
	       *index* query
	       #'(lambda (doc score)
		   (let ((document (montezuma::get-document *index* doc)))
		     (push (list :score (format nil "~,3F" score)
				 :date (get-field document "date")
				 :title (let ((title (get-field document "title")))
					  (if (= (length title) 0)
					      "[no title]"
					      title))
				 :id (tbnl:url-encode (get-field document "id"))
				 :link (cl-who:escape-string (get-field document "link")))
			   results)))
	       (list :num-docs 10 :first-doc start-doc))
	      (setf query-time (format nil "~,3F" (/ (- (get-internal-real-time) start-time)
						     internal-time-units-per-second))))
	    (setf num-results (format nil "~A" (length results)))
	    (setf results (reverse results)))))
      (format T "~&Results: ~S, start-doc: ~S" results start-doc)
      (with-output-to-string (s)
	(html-template:fill-and-print-template
	 main-tmpl
	 `(:query-string ,escaped-query-string
	   :query-object ,query-object
	   :num-results ,num-results
	   :result-span ,(format nil "Displaying results ~S to ~S." (+ start-doc 1) (+ start-doc (length results)))
	   :start-doc ,(format nil "~A" (+ start-doc 10))
	   :query-time ,query-time
	   :index-size ,(format nil "~A" (montezuma::size *index*))
	   :results ,results)
	 :stream s)))))
