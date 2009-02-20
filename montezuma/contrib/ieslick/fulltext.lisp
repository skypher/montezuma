(in-package :lamsight2)

(lamsight2-proclamations)

;; ===================================================
;;  Wrapper support to Montezuma full text indexing
;; ===================================================

(defvar *fulltext-index* nil)

(defvar *fulltext-lock* nil
  "MP lock for side effecting montezuma index")

(defmacro fulltext-lock (single-expr)
  `(hunchentoot-mp:with-lock (*fulltext-lock*)
     ,single-expr))

;; ==================================================
;;  API: Open and close the main index
;; ==================================================

(defun open-lamsight2-fulltext-index ()
  (open-fulltext-index (lamsight2-fulltext-index-path)))

(defun close-lamsight2-fulltext-index ()
  (close-fulltext-index))

(defun wipe-lamsight2-fulltext-index ()
  (wipe-fulltext-index (lamsight2-fulltext-index-path)))

(eval-when (:load-toplevel)
  (add-hook :start-app :init-fulltext-index
	    'open-lamsight2-fulltext-index)
  (add-hook :stop-app :close-fulltext-index
	    'close-lamsight2-fulltext-index))

(defun lamsight2-fulltext-index-path ()
  (make-pathname :directory (lamsight-relative-path (list "data"))
		 :name "fulltext"
		 :type "idx"))

(defun reindex-lamsight2-from-scratch ()
  (ignore-errors (close-lamsight2-fulltext-index))
  (handler-case 
      (wipe-lamsight2-fulltext-index)
    (error ()
      (cerror "Continue?" "Please wipe fulltext index manually before continuing")))
  (ignore-errors (open-lamsight2-fulltext-index))
  (mapcar #'index-object (get-instances-by-class 'question))
  (mapcar #'index-object (get-instances-by-class 'survey)))

;; ==================================================
;;  Generalized fulltext index interface  
;; ==================================================

(defun open-fulltext-index (path)
  "Assumes a single thread starting up at once"
  (when *fulltext-index*
    (warn "A fulltext index already exists; ignoring.")
    (return-from open-fulltext-index))
  (unless *fulltext-lock*
    (setf *fulltext-lock*
	  (hunchentoot-mp:make-lock "montezuma")))
  (setf *fulltext-index* 
	(make-instance 'montezuma:index :path path :default-search-field "*")))

(defun wipe-fulltext-index (path)
  (close-fulltext-index)
  (delete-file path))

(defun close-fulltext-index ()
  (when *fulltext-index*
    (montezuma:close *fulltext-index*)
    (setf *fulltext-index* nil)))

;; ===================================================
;;  Simple object indexing interface - supports mixin
;; ===================================================

(defun index-object (object)
  (awhen (fulltext-document object)
    (montezuma:add-document-to-index *fulltext-index* it)))

(defun unindex-object (object)
  (awhen (montezuma:get-document *fulltext-index* (object-id object))
    (montezuma:delete-document *fulltext-index* it)))

;; Support for auto-indexing objects

(defgeneric fulltext-document (object)
  (:documentation "Allows a class to define an auto create function to
   use the simple object creation method for fulltext indexing")
  (:method ((object t))
    (error "Must define 'fulltext-document' to index object ~A" object)))

(defun auto-create-fulltext-document (inst id-fn field-descriptions &key store-values)
  "Create a montezuma document from an instance and description; assumes
   that the object can be recreated using the id and class.  Values are
   not stored unless explicitly requested in the field descriptions or 
   top-level store-values keyword argument"
  (let ((doc (make-instance 'montezuma:document)))
    (labels ((add (name value &key stored tokenized binary)
	       (montezuma:add-field doc (field name value stored tokenized binary)))
	     (field (name value stored tokenized binary)
	       (montezuma:make-field name value
				     :stored stored
				     :index (if tokenized 
						:tokenized
						:untokenized)
				     :binary-p binary)))
						
      (add "id" (format nil "~A" (funcall id-fn inst)) :stored t)
      (add "class" (canonicalize-symbol (class-name (class-of inst)))
	   :stored t)
      (dolist (field-desc field-descriptions)
	(destructuring-bind (name &key stored untokenized) field-desc
	  (when (slot-boundp inst name)
	    (add (canonicalize-symbol name)
		 (slot-value inst name)
		 :stored (or stored store-values)
		 :tokenized (not untokenized)))))
      doc)))
 

;; ==================================================
;;  API: Fulltext query interface
;; ==================================================

(defun fulltext-search (query &key (obj-fn #'return-object-by-id)
			primary-fn)
  "Basic lucene queries; returns a list of matching objects in score order"
  (let ((results nil)
	(query-obj (create-fulltext-query query)))
    (labels ((collect (id score)
	       (declare (ignore score))
	       (push (funcall obj-fn 
			      (parse-integer
			       (montezuma:document-value 
				(montezuma:get-document *fulltext-index* id) 
				"id")))
		     results)))
      (montezuma:search-each *fulltext-index* query-obj (or primary-fn #'collect))
      results)))
  
(defun fulltext-ids-search (query)
  "Perform simple google-like text queries"
  (fulltext-search query :obj-fn #'identity))

(defun fulltext-class-search (query classes)
  "Perform simple google-like text queries over a set of classes"
  (mapcan #'(lambda (class)
	      (fulltext-search (format nil "+class:~A ~{+~A ~}"
				       (convert-class class)
				       (split-sequence:split-sequence #\space query))))
	  (mklist classes)))

(defun fulltext-field-search (query class field)
  "Search just the field of the requested class"
  (fulltext-class-search (concatenate 'string "+" (canonicalize-symbol field) 
				      ":\"" query "\"")
			 class))

;;
;; Query utilities
;;

(defparameter *analyzer* (make-instance 'montezuma:standard-analyzer))

(defun create-fulltext-query (string)
  string)

;;
;; Search utilities
;;

(defun return-object-by-id (id)
  "Reconstitute an object from its id"
  (find-persistent-object-by-id *default-store* nil id))

(defun return-object (id)
  "In case you aren't using the elephant data store - not really supported for now"
  (let ((doc (montezuma:get-document *fulltext-index* id)))
    (find-persistent-object-by-id *default-store* (montezuma:document-value doc "*class") id)))

(defun canonicalize-symbol (symbol)
  (string-downcase (symbol-name symbol)))

(defun convert-escapes (name)
  (substitute #\? #\- name))

(defun convert-class (classname)
  (convert-escapes (canonicalize-symbol classname)))
  
