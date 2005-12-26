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


;; FIXME need test-term-buffer-read
