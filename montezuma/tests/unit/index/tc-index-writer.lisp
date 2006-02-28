(in-package #:montezuma)

(defun set= (a b &key (test #'eql))
  (and (null (set-difference a b :test test))
       (null (set-difference b a :test test))))

(deftestfixture index-writer-test
    (:vars dir ir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-index-writer-initialize
    (flet ((bool= (a b) (or (and a b) (and (not a) (not b)))))
      (let* ((dir (fixture-var 'dir))
	     (iw (make-instance 'index-writer
				:directory (fixture-var 'dir)
				:create-p T)))
	(atest index-writer-initialize-1 (file-exists-p dir "segments") T #'bool=)
	(close iw)
	(atest index-writer-initialize-2 (file-exists-p dir "segments") T #'bool=))))
  (:testfun test-index-writer-add-document
    (let* ((dir (fixture-var 'dir))
	   (iw (make-instance 'index-writer
			      :directory dir
			      :analyzer (make-instance 'standard-analyzer)
			      :create-p T))
	   (doc (index-test-helper-prepare-document))
	   (infos (make-instance 'field-infos)))
      (add-doc-fields infos doc)
      (add-document-to-index-writer iw doc)
      (atest index-writer-add-document-1 (document-count iw) 1)
      (close iw)))
  (:testfun test-index-writer-add-documents
    (let* ((dir (fixture-var 'dir))
	   (iw (make-instance 'index-writer
			      :directory dir
			      :analyzer (make-instance 'standard-analyzer)
			      :create-p T))
	   (docs (index-test-helper-prepare-book-list))
	   (infos (make-instance 'field-infos)))
      (setf (merge-factor iw) 3)
      (setf (min-merge-docs iw) 3)
      (add-doc-fields infos (elt docs 0))
      (dosequence (doc docs)
	(add-document-to-index-writer iw doc))
      (atest index-writer-add-documents-1 (document-count iw) 37)
      (atest index-writer-add-documents-2
	     (files dir)
	     '("segments" "_1g.cfs" "_13.cfs" "deletable")
	     #'set=)
      (close iw))))



	