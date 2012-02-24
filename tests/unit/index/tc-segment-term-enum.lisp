(in-package #:montezuma)

(deftestfixture segment-term-enum
  (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-segment-term-enum-initialize
	    (let ((fis (make-instance 'field-infos))
		  (dir (fixture-var 'dir)))
	      (add-field-info fis "author" :indexed-p T :store-term-vector T)
	      (add-field-info fis "title" :indexed-p T :store-term-vector T)
	      (let ((term-infos (loop for i from 0 to 4
				   collect (make-instance 'term-info
							  :doc-freq i
							  :freq-pointer i
							  :prox-pointer i
							  :skip-offset 0)))
		    (terms (list
			    (make-term "author" "Martel")
			    (make-term "title" "Life of Pi")
			    (make-term "author" "Martin")
			    (make-term "title" "Life on the edge"))))
		(setf terms (cl:sort terms #'term<))
		(let ((tiw (make-instance 'term-infos-writer
					  :directory dir
					  :segment "_test"
					  :field-infos fis
					  :interval 128)))
		  (dotimes (i 4)
		    (add-term tiw (elt terms i) (elt term-infos i)))
		  (close tiw))
		(let* ((tis-file (open-input dir "_test.tis"))
		       (ste (make-instance 'segment-term-enum
					   :input tis-file
					   :field-infos fis
					   :is-index NIL)))
		  (test segment-term-enum-init-1 (index-interval ste) 128)
		  (test segment-term-enum-init-2 (skip-interval ste) 16)
		  (test segment-term-enum-init-3 (size ste) 4)
		  (test segment-term-enum-init-4 (and (next? ste) T) T)
		  (test segment-term-enum-init-5 (term ste) (elt terms 0) #'term=)
		  (test segment-term-enum-init-6 (term-info ste) (elt term-infos 0) #'term-info=)
		  (let ((ti (make-instance 'term-info)))
		    (setf (term-info ste) ti)
		    (test segment-term-enum-init-7 ti (elt term-infos 0) #'term-info=))
		  (test segment-term-enum-init-8 (and (next? ste) T) T)
		  (test segment-term-enum-init-9 (previous ste) (elt terms 0) #'term=)
		  (test segment-term-enum-init-10 (term ste) (elt terms 1) #'term=)
		  (test segment-term-enum-init-11 (term-info ste) (elt term-infos 1) #'term-info=)
		  (test segment-term-enum-init-12 (and (next? ste) T) T)
		  (test segment-term-enum-init-13 (previous ste) (elt terms 1) #'term=)
		  (test segment-term-enum-init-14 (term ste) (elt terms 2) #'term=)
		  (test segment-term-enum-init-15 (term-info ste) (elt term-infos 2) #'term-info=)
		  (test segment-term-enum-init-16 (and (next? ste) T) T)
		  (test segment-term-enum-init-17 (previous ste) (elt terms 2) #'term=)
		  (test segment-term-enum-init-18 (term ste) (elt terms 3) #'term=)
		  (test segment-term-enum-init-19 (term-info ste) (elt term-infos 3) #'term-info=)
		  (close ste))
		(let* ((tii-file (open-input dir "_test.tii"))
		       (ste (make-instance 'segment-term-enum
					   :input tii-file
					   :field-infos fis
					   :is-index NIL)))
		  (test segment-term-enum-init-20 (index-interval ste) 128)
		  (test segment-term-enum-init-21 (skip-interval ste) 16)
		  (test segment-term-enum-init-22 (size ste) 1)
		  (test segment-term-enum-init-23 (and (next? ste) T) T)
		  (test segment-term-enum-init-24 (term ste) (make-term "" "") #'term=))))))
