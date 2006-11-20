(cl:defpackage #:reuters-indexer
  (:use #:common-lisp))

(in-package #:reuters-indexer)

(defvar *index-path*
  (merge-pathnames (make-pathname :directory '(:relative "montezuma_index"))
		   (make-pathname :name nil
				  :type nil
				  :defaults *load-pathname*)))

(defvar *archive-path*
  (merge-pathnames (make-pathname :directory '(:relative "corpus"))
		   (make-pathname :name nil
				  :type nil
				  :defaults *load-pathname*)))

(defun init-writer (create-p)
  (make-instance 'montezuma:index
		 :path *index-path*
		 :analyzer (make-instance 'montezuma:whitespace-analyzer)
		 :merge-factor 100
		 :use-compound-file-p T
		 :create-p create-p))

(defun build-index (file-list max-to-index increment store-p)
  (let ((writer (init-writer T))
	(docs-so-far 0))
    (dolist (file file-list)
      (with-open-file (in file :direction :input :external-format :latin-1)
	(let ((title (read-line in)))
	  (assert title)
	  (let ((doc (make-instance 'montezuma:document))
		(body (montezuma::stream-contents in)))
	    (montezuma:add-field doc (montezuma:make-field :title title
							   :index :tokenized
							   :stored store-p
							   :store-term-vector store-p))
	    (montezuma:add-field doc (montezuma:make-field :body body
							   :index :tokenized
							   :stored store-p
							   :store-term-vector store-p))
	    (montezuma:add-document-to-index writer doc))))
      (incf docs-so-far)
      (when (zerop (mod docs-so-far 1000))
	(format T ".")
	(force-output))
      (when (>= docs-so-far max-to-index)
	(return-from build-index nil))
      (when (and increment (> increment 0) (zerop (mod docs-so-far increment)))
	(montezuma:close writer)
	(setf writer (init-writer NIL))))
    (let ((num-indexed (montezuma::document-count writer)))
      (montezuma:optimize writer)
      (montezuma:close writer)
      num-indexed)))


(defun all-articles ()
  (let ((files '()))
    (cl-fad:walk-directory *archive-path*
			   #'(lambda (path)
			       (when (string= (pathname-type path) "txt")
				 (push path files))))
    (reverse files)))


(defun runit ()
  (format T "~&------------------------------------------------------------~%")
  (let ((files (all-articles)))
    (let ((docs (length files))
	  (reps 1)
	  (inc 0)
	  (store-p NIL)
	  (times '()))
      (dotimes (i reps)
	(let ((start (get-internal-run-time)))
	  (let ((num-indexed (build-index files docs inc store-p)))
	    (let ((duration (/ (- (get-internal-run-time) start) internal-time-units-per-second)))
	      (format T "~&~s  Secs: ~,2F  Docs: ~S" i duration num-indexed)
	      (push duration times))))))))

  