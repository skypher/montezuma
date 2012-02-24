(in-package #:montezuma)

(deftestfun test-priority-queue
  (let ((pq (make-instance 'priority-queue
			   :max-size 4
			   :predicate #'string<)))
    (queue-push pq "bword")
    (test priority-queue-1 (size pq) 1)
    (test priority-queue-2 (queue-top pq) "bword" #'string=)
    (queue-push pq "cword")
    (test priority-queue-3 (size pq) 2)
    (test priority-queue-4 (queue-top pq) "bword" #'string=)
    (queue-push pq "aword")
    (test priority-queue-5 (size pq) 3)
    (test priority-queue-6 (queue-top pq) "aword" #'string=)
    (queue-push pq "dword")
    (test priority-queue-7 (size pq) 4)
    (test priority-queue-8 (queue-top pq) "aword" #'string=)
    (test priority-queue-9 (queue-pop pq) "aword" #'string=)
    (test priority-queue-10 (size pq) 3)
    (test priority-queue-11 (queue-pop pq) "bword" #'string=)
    (test priority-queue-12 (size pq) 2)
    (test priority-queue-13 (queue-pop pq) "cword" #'string=)
    (test priority-queue-14 (size pq) 1)
    (test priority-queue-15 (queue-pop pq) "dword" #'string=)
    (test priority-queue-16 (size pq) 0)))

(deftestfun test-priority-queue-clear
  (let ((pq (make-instance 'priority-queue
			   :max-size 3
			   :predicate #'string<)))
    (queue-push pq "word1")
    (queue-push pq "word2")
    (queue-push pq "word3")
    (test priority-queue-clear-1 (size pq) 3)
    (queue-clear pq)
    (test priority-queue-clear-2 (size pq) 0)))

(deftestfun test-priority-queue-adjust-top
  (let ((pq (make-instance 'priority-queue
			   :max-size 100
			   :predicate #'(lambda (a b) (< (elt a 0) (elt b 0))))))
    (dotimes (i 100)
      (queue-push pq (vector (random 1000))))
    (dotimes (i 100)
      (setf (elt (queue-top pq) 0) (random 1000))
      (adjust-top pq))
    (let ((prev (queue-pop pq))
	  (curr (queue-pop pq))
	  (success T))
      (while curr
	(when (> (elt prev 0) (elt curr 0))
	  (setf success NIL))
	(setf curr (queue-pop pq)))
      (test priority-queue-adjust-top-1 success T))))
    

(deftestfun test-priority-queue-stress
  (let ((pq (make-instance 'priority-queue
			   :max-size 100
			   :predicate #'<)))
    (dotimes (i 100)
      (queue-push pq (random 100)))
    (let ((prev (queue-pop pq))
	  (success T))
      (dotimes (i 99)
	(let ((curr (queue-pop pq)))
	  (when (> prev curr)
	    (setf success NIL))
	  (setf prev curr)))
      (test priority-queue-stress-1 success T)))
  (let ((pq (make-instance 'priority-queue
			   :max-size 1000
			   :predicate #'<)))
    (dotimes (i 100)
      (dotimes (j 10)
	(queue-push pq (+ (* i 5) (- j 5)))))
    (let ((prev (queue-pop pq))
	  (curr (queue-pop pq))
	  (success T))
      (while curr
	(when (> prev curr)
	  (setf success NIL))
	(setf curr (queue-pop pq)))
      (test priority-queue-stress-2 success T))))

