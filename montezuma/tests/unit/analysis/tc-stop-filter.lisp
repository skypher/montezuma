(in-package #:montezuma)


;; This seems kind of ridiculous.  See
;; <http://meme.b9.com/cview.html?channel=lisp&utime=3344192236#utime_requested>;
;; even Lisp implementors find it surprising.

(defparameter *word-file-path* (make-pathname :name "wordfile" :type nil
					      :defaults
					      (merge-pathnames (make-pathname :directory '(:relative "data"))
							       *load-pathname*)))


(deftestfun test-stop-filter
  (with-input-from-string (input "The Quick AND the DEAD the and to it there their")
    (let ((filter (make-instance 'stop-filter
				 :input (make-instance 'lowercase-tokenizer
						       :input input)
				 :file *word-file-path*)))
      (test stop-filter-1 (next-token filter) (make-token "quick" 4 9) #'token=)
      (test stop-filter-2 (next-token filter) (make-token "dead" 18 22) #'token=)
      (test stop-filter-3 (next-token filter) nil))))

      