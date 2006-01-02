(in-package #:montezuma)

(deftestfixture compound-file-io
    (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-compound-file-writer
	    (let ((dir (fixture-var 'dir)))
	      (let ((file1 (create-output dir "file1"))
		    (file2 (create-output dir "file2")))
		(write-int file1 20)
		(write-string file2 "this is file2")
		(close file1)
		(close file2))
	      (let ((cfile-writer (make-instance 'compound-file-writer
						 :directory dir
						 :file-name "cfile")))
		(add-file cfile-writer "file1")
		(add-file cfile-writer "file2")
		(close cfile-writer))
	      (let ((cfile (open-input dir "cfile")))
		(test compound-file-writer-1 (read-vint cfile) 2)
		(test compound-file-writer-2 (read-long cfile) 29)
		(test compound-file-writer-3 (read-string cfile) "file1" #'string=)
		(test compound-file-writer-4 (read-long cfile) 33)
		(test compound-file-writer-5 (read-string cfile) "file2" #'string=)
		(test compound-file-writer-6 (read-int cfile) 20)
		(test compound-file-writer-7 (read-string cfile) "this is file2" #'string=)))))

		