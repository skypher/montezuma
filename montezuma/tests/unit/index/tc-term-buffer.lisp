(in-package #:montezuma)

(deftestfun test-term-set
  (let ((term (make-term "title" "something or other"))
	(tb (make-instance 'term-buffer)))
    (setf (term tb) term)
    (test test-term-buffer-1 (term-field term) (field tb) #'string=)
    (test test-term-buffer-2 (text tb) "something or other" #'string=)
    (test test-term-buffer-3 (text-length tb) (length "something or other"))
    (test test-term-buffer-4 (term tb) term #'term=)))

(deftestfun test-term-buffer-compare
  (let ((tb1 (make-instance 'term-buffer))
	(tb2 (make-instance 'term-buffer)))
    (setf (term tb1) (make-term "alpha" "text"))
    (setf (term tb2) (make-term "bravo" "text"))
    (test term-buffer-compare-1 (and (term-buffer< tb1 tb2) T) T)
    (setf (term tb2) (make-term "alpha" "text"))
    (test term-buffer-compare-2 (and (term-buffer= tb1 tb2) T) T)
    (setf (term tb2) (make-term "alpha" "tex"))
    (test term-buffer-compare-3 (and (term-buffer> tb1 tb2) T) T)))

(deftestfun test-term-buffer-read
  (let ((dir (make-instance 'ram-directory))
	(fis (make-instance 'field-infos))
	(tb (make-instance 'term-buffer)))
    (setf (term tb) (make-term "Author" "John"))
    (add-field-info fis "Writer" :indexed-p T)
    (let ((output (create-output dir "term-buffer-read-test")))
      (write-vint output 4)
      (write-vint output 8)
      (write-chars output (string-to-bytes " Wiseman") 0 8)
      (write-vint output (get-field-number fis "Writer"))
      (close output))
    (let ((input (open-input dir "term-buffer-read-test")))
      (read-term-buffer tb input fis)
      (test term-buffer-read-1 (text tb) "John Wiseman" #'string=)
      (test term-buffer-read-2 (term-text (term tb)) "John Wiseman" #'string=)
      (test term-buffer-read-3 (field tb) "Writer" #'string=))))

(deftestfun test-term-buffer-merge
  (let ((i (make-instance 'index)))
    (dotimes (j 333) ; induce term merging process
      (add-document-to-index i "über")
      (add-document-to-index i '((id . 0)("test"))))))

