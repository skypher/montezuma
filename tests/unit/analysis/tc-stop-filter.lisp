(in-package #:montezuma)

(defun asdf-system-directory (asdf-system-name)
  "Computes the directory in which the .asdf file for a given ASDF
  system resides."
  (make-pathname :directory
                 (pathname-directory (truename (asdf:system-definition-pathname
                                                 (asdf:find-system asdf-system-name))))))

(defun montezuma.asd-relative (path)
  (merge-pathnames path (asdf-system-directory :montezuma)))

;; This seems kind of ridiculous.  See
;; <http://meme.b9.com/cview.html?channel=lisp&utime=3344192236#utime_requested>;
;; even Lisp implementors find it surprising.

(defparameter *word-file-path* (montezuma.asd-relative
                                 (make-pathname :name "wordfile" :type nil
                                                :defaults
                                                (make-pathname :directory '(:relative "tests/unit/analysis/data")))))


(deftestfun test-stop-filter
  (with-input-from-string (input "The Quick AND the DEAD the and to it there their")
    (let ((filter (make-instance 'stop-filter
				 :input (make-instance 'lowercase-tokenizer
						       :input input)
				 :file *word-file-path*)))
      (test stop-filter-1 (next-token filter) (make-token "quick" 4 9) #'token=)
      (test stop-filter-2 (next-token filter) (make-token "dead" 18 22) #'token=)
      (test stop-filter-3 (next-token filter) nil))))

      
