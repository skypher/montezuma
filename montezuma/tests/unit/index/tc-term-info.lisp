(in-package #:montezuma)

(deftestfun test-term-info
  (let ((ti-1 (make-instance 'term-info
			     :doc-freq 1
			     :freq-pointer 2
			     :prox-pointer 3
			     :skip-offset 1)))
    (test term-info-1 (doc-freq ti-1) 1)
    (test term-info-2 (freq-pointer ti-1) 2)
    (test term-info-3 (prox-pointer ti-1) 3)
    (test term-info-4 (skip-offset ti-1) 1)
    (let ((ti-2 (clone ti-1)))
      (test term-info-5 (and (term-info= ti-1 ti-2) T) T)
      (setf ti-2 (make-instance 'term-info
				:doc-freq 10
				:freq-pointer 9
				:prox-pointer 8))
      (test term-info-6 (term-info= ti-1 ti-2) NIL)
      (set-from-term-info ti-2 ti-1)
      (test term-info-7 (term-info= ti-1 ti-2) T))))
