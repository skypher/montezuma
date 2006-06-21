;; Example code showing how to use Montezuma to index and search
;; Lisppastes.
;;
;; Example usage:
;;
;;   CL-USER> (use-package '#:paste-search)
;;   T
;;   CL-USER> (index-pastes)
;;   Indexing 1000 pastes... Indexing took 55.638 seconds.
;;   Optimizing... Optimizing took 30.613 seconds.
;;   ; No value
;;   CL-USER> (search-pastes "public class")
;;   Score  Date                 #      Channel     User             Title
;;   -----  -------------------  -----  ----------  ---------------  -----
;;    1.36  2006-05-01 11:35:55  19570  None        davidhouse       my ssh public key
;;    1.22  2006-04-10 22:42:12  18846  #haskell    RyanT5000        Skin class
;;    1.10  2006-04-25 17:54:59  19366  #programmering  lnostdal         calling constructor of base class
;;    1.03  2006-04-21 15:55:12  19232  #haskell    Cale             Fst and Snd class
;;    0.93  2006-05-08 20:47:41  19820  None        Zarutian         part of SEXPparser class
;;    0.93  2006-05-08 20:43:10  19819  None        Zarutian         part of SEXPparser class
;;    0.93  2006-05-04 16:07:26  19685  #haskell    kombinator       no class functions
;;    0.41  2006-05-02 12:34:28  19619  None        b42              error: looser throw specifier for ...
;;    0.41  2006-05-02 12:14:46  19618  None        b42              error: looser throw specifier for ...
;;    0.35  2006-04-19 15:18:20  19156  None        vampire0         Test.java
;;   10 results in 0.050 seconds.
;;   NIL
;;   CL-USER> (get-paste-contents 19156)
;;   "public class Test {
;;      public static void testMethod(String test) {
;;          System.out.println(\"void: \"+test);
;;      }
;;        public static String testMethod(String test) {
;;          System.out.println(\"String: \"+test);
;;          return test;
;;      }
;;        public static void main(String[] args) {
;;          testMethod(\"testVoid\");
;;          String test = testMethod(\"testString\");
;;          System.out.println(\"main: \"+test);
;;      }
;;   }
;;   "
;;
;; The corpus supplied with Montezuma consists of 1000 pastes from
;; <http://paste.lisp.org/>.  Each paste consists of a number, user,
;; date, channel, title and contents (pastes also have annotations,
;; but I ignored them for this example).
;;
;; To build or rebuild the index of all 1000 pastes from scratch:
;;
;;   CL-USER> (index-pastes)
;;
;; To add a single new paste to the index:
;;
;;   CL-USER> (add-paste (make-paste :number 17
;;                                   :user "jimbo"
;;                                   :date (get-universal-time)
;;                                   :channel "#web4.0"
;;                                   :title "buttocks"
;;                                   :contents "( | )"))
;;
;; To delete a paste from the index:
;;
;;    CL-USER> (delete-paste 17)
;;
;; To optimize the index for maximum query performance after a large
;; number of additions, deletions or other updates to the index:
;;
;;   CL-USER> (optimize-index)

(cl:defpackage #:paste-search
  (:use #:common-lisp)
  (:export #:*pastes-path*
	   #:*index-path*
	   #:index-pastes
	   #:delete-paste
	   #:add-paste
	   #:optimize-index
	   #:search-pastes
	   #:get-paste-contents
	   #:make-paste))

(in-package #:paste-search)


;; -- Variables

(defparameter *pastes-path*
  (make-pathname :name "pastes"
		 :type "sexp"
		 :defaults *load-pathname*)
  "The directory containing the Lisppaste archives.")

(defvar *index-path*
  (merge-pathnames (make-pathname :directory '(:relative "pasteindex"))
		   (make-pathname :name nil
				  :type nil
				  :defaults *load-pathname*))
  "The path at which to store the index.")

(defvar *index* nil
  "The Montezuma index for the Lisppaste archives.")


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

(defstruct paste
  number
  user
  date
  channel
  title
  contents
  annotations)


(defun index-pastes ()
  "Completely indexes all pastes from scratch."
  (let ((index (make-instance 'montezuma:index
			      :path *index-path*
			      :create-p T
			      ;; Setting :min-merge-docs to 5000 helps
			      ;; keep most of the indexing in RAM.
			      ;; You might want to decrease this if
			      ;; you're RAM-starved.
			      :min-merge-docs 5000))
	(pastes (with-open-file (in *pastes-path* :direction :input)
		  (cl:read in))))
      (format T "~&Indexing ~S pastes... " (length pastes))
      (time-form
       "Indexing took ~,3F seconds."
       (dolist (paste pastes)
	 (index-paste index paste)))
      (format T "~&Optimizing... ")
      (time-form "Optimizing took ~,3F seconds."
		 (montezuma:optimize index))
      (montezuma:close index)
      (values)))


(defun index-paste (index paste)
  ;; Each paste is a separate document.
  ;;
  ;; For simplicity, this example assumes there is no external
  ;; database or other storage of paste information, so many fields
  ;; are not just indexed by Montezuma but stored so they can be
  ;; retrieved for the display of search results.  If you were using a
  ;; database of some sort you would only have to store the database
  ;; ID in the Montezuma index which would allow you to lookup paste
  ;; information when displaying query results.
  (let ((doc (make-instance 'montezuma:document)))
    ;; We want to be able to search on the ID field, but it should not
    ;; be tokenized.
    (montezuma:add-field doc (montezuma:make-field "number" (format nil "~S" (paste-number paste))
						   :index :untokenized))
    ;; The USER field is stored and tokenized.
    (montezuma:add-field doc (montezuma:make-field "user" (paste-user paste)
						   :index :tokenized))
    ;; Convert dates from universal times to strings like
    ;; "2006-06-06".  Because of the way Montezuma does DATE queries
    ;; (the secret is that they're no different from any other query),
    ;; there is some advantage in decreasing the number of unique
    ;; terms, so we limit the resolution to one day.
    (montezuma:add-field doc (montezuma:make-field "date" (date-string (paste-date paste))
						   :index :untokenized :stored NIL))
    ;; The DATEDISPLAY field is not meant to be queried, it exists only to
    ;; show search results with timestamps of a nice 1 second resolution, like
    ;; "2006-06-06 23:59:59".
    (montezuma:add-field doc (montezuma:make-field "displaydate" (timestamp-string (paste-date paste))
						   :index :untokenized))
    ;; Store and tokenize the CHANNEL, TITLE and CONTENTS fields.
    (montezuma:add-field doc (montezuma:make-field "channel" (paste-channel paste)
						   :index :tokenized))
    (montezuma:add-field doc (montezuma:make-field "title" (paste-title paste)
						   :index :tokenized))
    (montezuma:add-field doc (montezuma:make-field "contents" (paste-contents paste)
						   :index :tokenized))
    ;; Finally, add the document to the index.
    (montezuma:add-document-to-index index doc)))


(defun delete-paste (number)
  "Deletes the specified paste from the index."
  (unless *index*
    (load-index))
  (montezuma:delete *index* (montezuma:make-term "number" (format nil "~A" number)))
  (montezuma:flush *index*))


(defun add-paste (paste)
  "Adds a paste to the index."
  (unless *index*
    (load-index))
  (index-paste *index* paste)
  (montezuma:flush *index*))


(defun optimize-index ()
  "Optimizes the index for best query performance."
  (unless *index*
    (load-index))
  (montezuma:optimize *index*))


(defun load-index ()
  (setf *index* (make-instance 'montezuma:index
			       :path *index-path*
			       :create-p NIL
			       :create-if-missing-p NIL
			       ;; Unless otherwise specified, queries
			       ;; will search all these fields
			       ;; simultaneously.
			       :default-field "*"
			       :fields '("user" "channel" "title" "contents"))))


;; -- Searching

;; Example searches:
;;
;; (search-pastes "public class")
;; (search-pastes "+user:lemonodor +date:2006-04*")
;; (search-pastes "user:lemonodor" '(:num-docs 10000) T)
;; (search-pastes "bug*" '(:num-docs 10))
;; (search-pastes "bug*" '(:num-docs 10 :first-doc 10))
;; (search-pastes "date:2006-05*" '(:num-docs 10000) T)
;; (search-pastes "channel:lisp" '(:num-docs 10000) T)

(defun search-pastes (query &optional options count-only-p)
  "Searches the paste index."
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
	(print-results (reverse results)))
      (format T "~&~S results in ~,3F seconds." (length results) time))))


(defun print-results (results)
  (format T "~&~5A  ~19A  ~5A  ~10A  ~15A  ~A" "Score" "Date" "#" "Channel" "User" "Title")
  (format T "~&-----  -------------------  -----  ----------  ---------------  -----")
  (dolist (result results)
    (let ((doc (car result))
	  (score (cdr result)))
      (let ((paste (montezuma:get-document *index* doc)))
	(flet ((get-field (name)
		 (montezuma:field-data (montezuma:document-field paste name))))
	  (format T "~&~5,2F  ~A  ~A  ~10A  ~15A  ~A"
		  score
		  (get-field "displaydate")
		  (get-field "number")
		  (get-field "channel")
		  (get-field "user")
		  (get-field "title")))))))


(defun get-paste-contents (number)
  "Returns the contents of the specified paste."
  (unless *index*
    (load-index))
  (let ((doc (montezuma:get-document *index*
				     (montezuma:make-term "number" (format nil "~A" number)))))
    (if doc
	(montezuma:field-data (montezuma:document-field doc "contents"))
	(error "Paste #~S could not be found." number))))


;; -- Misc.

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
