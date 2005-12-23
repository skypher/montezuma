(in-package #:montezuma)

(deftestfun test-ram-store
  (test-basic-file-ops (make-instance 'ram-directory))
  (test-rename (make-instance 'ram-directory))
  (test-modified (make-instance 'ram-directory))
  (test-rw-bytes (make-instance 'ram-directory)))


#|
(deftestfun test-ram-lock
  (let ((dir (make-instance 'ram-directory)))
    (let* ((name "lfile")
	   (lock-file (format nil "~A~A" (lock-prefix dir) name)))
      (test ram-lock-1 (file-exists-p dir lock-file) NIL)
      (let ((lock (make-lock dir name)))
	(test ram-lock-2 (file-exists-p dir lock-file) NIL)
	(test ram-lock-3 (locked-p lock) NIL)
	(obtain lock)
	(test ram-lock-4 (locked-p lock) T)
	(test ram-lock-5 (file-exists-p dir lock-file) T)
	(release lock)
	(test ram-lock-6 (locked-p lock) NIL)
	(test ram-lock-7 (file-exists-p dir lock-file) NIL)))))
|#
