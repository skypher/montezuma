(in-package #:montezuma)

(deftestfun test-term
  (let ((term-1 (make-term "bfield1" "athis is text1")))
    (test test-term-1 (term-field term-1) "bfield1" #'string=)
    (test test-term-2 (term-text term-1) "athis is text1" #'string=)
    (let ((term-2 (make-term "afield2" "athis is text1"))
	  (term-3 (make-term "bfield1" "bthis is text2"))
	  (term-4 (make-term "bfield1" "athis is text1")))
      (test test-term-3 (and (term> term-1 term-2) T) T)
      (test test-term-4 (and (term< term-1 term-3) T) T)
      (test test-term-5 (and (term< term-2 term-1 term-3) T) T)
      (test test-term-6 (and (term= term-1 term-4) T) T)
      (test test-term-7 (and (term= term-1 term-4 term-1 term-4) T) T)
      (test test-term-8 (and (not (term= term-1 term-4 term-1 term-4 term-2)) T) T)
      (setf (term-field term-4) "field3"
	    (term-text term-4) "text3")
      (test test-term-9 (and (not (term= term-1 term-4)) T) T))))
