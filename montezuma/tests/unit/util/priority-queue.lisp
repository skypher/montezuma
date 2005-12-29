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

    