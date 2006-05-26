(in-package #:montezuma)

(defparameter *corpus-path* (make-pathname :name nil
					   :type nil
					   :defaults *load-pathname*))

(defvar *paste-index* nil)
(defvar *pastes* nil)

(defun load-pastes ()
  (with-open-file (f (make-pathname :name "pastes" :type "db" :defaults *corpus-path*)
		     :direction :input)
    (setf *pastes* (cl:read f)))
  (length *pastes*))

(defun save-pastes ()
  (with-open-file (f (make-pathname :name "pastes" :type "db" :defaults *corpus-path*)
		     :direction :output
		     :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* T))
	(print *pastes* f))))
  (length *pastes*))

(defun load-paste-index ()
  (setf *paste-index* (make-instance 'index
				     :path (merge-pathnames
					    (make-pathname :directory '(:relative "pasteindex"))
					    *corpus-path*))))

(defstruct paste
  number
  user
  date
  channel
  title
  contents
  annotations)

#||
;; For grabbing pastes from the paste service at paste.lisp.org.

(require :s-xml-rpc)

(defun get-last-n-pastes (n)
  (let ((paste-headers
	 (s-xml-rpc:xml-rpc-call
	  (s-xml-rpc:encode-xml-rpc-call "pasteheaders" n)
	  :host "common-lisp.net"
	  :port 8185)))
    (loop for header in paste-headers
       collecting (get-paste-details (elt header 0)))))

(defun get-paste-details (n)
  (let ((p (s-xml-rpc:xml-rpc-call
	    (s-xml-rpc:encode-xml-rpc-call "pastedetails" n)
	    :host "common-lisp.net"
	    :port 8185)))
    (destructuring-bind (number xmldate user channel title num-annotations contents) p
      (make-paste :number number
		  :user user
		  :date (s-xml-rpc:xml-rpc-time-universal-time xmldate)
		  :channel channel
		  :title title
		  :contents contents))))
||#  


(defun index-pastes (&key (pastes *pastes*))
  (setf *paste-index* (make-instance 'index
				     :path (merge-pathnames
					    (make-pathname :directory '(:relative "pasteindex"))
					    *corpus-path*)
				     :default-field "contents"
				     :min-merge-docs 5000))
  (dolist (paste pastes)
    (index-paste *paste-index* paste))
  (optimize *paste-index*))

(defun date-string (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    year month date hour minute second)))


(defun index-paste (index paste)
  (let ((doc (make-instance 'document)))
    ;; I'm just using the paste format as returned by
    ;; paste.lisp.org's XML-RPC API.
    (let ((number   (make-field "id"   (format nil "~S" (paste-number paste))
				:index :untokenized :stored T))
	  (user     (make-field "user"     (paste-user paste)
				:index :untokenized :stored T))
	  (date     (make-field "date"     (date-string (paste-date paste))
				:index :untokenized :stored T))
	  (channel  (make-field "channel"  (paste-channel paste)
				:index :untokenized :stored T))
	  (title    (make-field "title"    (paste-title paste)
				:index :tokenized :stored T))
	  (contents (make-field "contents" (paste-contents paste)
				:stored NIL :index :tokenized)))
      (add-field doc number)
      (add-field doc user)
      (add-field doc date)
      (add-field doc channel)
      (add-field doc title)
      (add-field doc contents)
      (add-document-to-index *paste-index* doc))))

(defun search-pastes (field query &optional options)
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
    (search-each *paste-index* query
		 #'(lambda (doc score)
		     (when (= num-results 0)
		       (format T "~&~5A ~19A ~5A ~15A" "Score" "Date" "#" "User")
		       (format T "~&-------------------------------------------"))
		     (incf num-results)
		     (print-result doc score))
		 options)
    (format T "~&~%~S results displayed." num-results)))

(defun print-result (doc score)
  (let ((paste (get-document *paste-index* doc)))
    (format T "~&~5,2F ~A ~A ~15A~&~vt~A"
	    score
	    (field-data (document-field paste "date"))
	    (field-data (document-field paste "id"))
	    (field-data (document-field paste "user"))
	    10
	    (field-data (document-field paste "title")))))

(defun find-pastes (field words)
  (flet ((check (s)
	   (some #'(lambda (word)
		     (cl:search word (string-downcase s)))
		 words)))
    (dosequence (paste *pastes* :index i)
      (when (check (funcall field paste))
	(format T "~&~S" i)))))


(defun tokens (string)
  (let ((a (make-instance 'standard-analyzer)))
    (let ((ts (token-stream a "contents" string)))
      (let ((token (next-token ts))
	    (tokens '()))
	(while token
	  (push token tokens)
	  (setf token (next-token ts)))
	(reverse tokens)))))


(require :json)

(use-package :json)

(defmethod encode-json ((p paste) stream)
  (let ((props `((:number . ,(paste-number p))
		 (:user . ,(paste-user p))
		 (:date . ,(paste-date p))
		 (:channel . ,(paste-channel p))
		 (:title . ,( paste-title p))
		 (:contents . ,(paste-contents p)))))
    (encode-json-alist props stream)))

(defun write-json-pastes ()
  (with-open-file (f (make-pathname :name "pastes" :type "json" :defaults *corpus-path*)
		     :direction :output
		     :if-exists :supersede)
    (encode-json *pastes* f)))

