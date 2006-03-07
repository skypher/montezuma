(in-package #:montezuma)

(deftestfixture compound-file-io-writer
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


(deftestfixture compound-file-io-reader
  (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-compound-file-reader
   (let ((dir (fixture-var 'dir)))
     (let ((cfile (create-output dir "cfile")))
       (write-vint cfile 2)
       (write-long cfile 29)
       (write-string cfile "file1")
       (write-long cfile 33)
       (write-string cfile "file2")
       (write-int cfile 20)
       (write-string cfile "this is file 2")
       (close cfile))
     (let ((cfile-reader (make-instance 'compound-file-reader
					:directory dir
					:file-name "cfile")))
       (test compound-file-reader-1 (file-size cfile-reader "file1") 4)
       (test compound-file-reader-2 (file-size cfile-reader "file2") 15)
       (let ((file1 (open-input cfile-reader "file1"))
	     (file2 (open-input cfile-reader "file2")))
	 (test compound-file-reader-3 (read-int file1) 20)
	 (test compound-file-reader-4 (read-string file2) "this is file 2" #'string=)
	 (close file1)
	 (close file2))))))

(deftestfixture compound-file-io-io
  (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-compound-file-buffer
   (let ((dir (fixture-var 'dir))
	 (big-string (with-output-to-string (s)
		       (dotimes (i 1000)
			 (format s "this is file2")))))
     (let ((file1 (create-output dir "file1"))
	   (file2 (create-output dir "file2"))
	   (file3 (create-output dir "file3")))
       (dotimes (i 20)
	 (write-int file1 (random 10000)))
       (write-string file2 big-string)
       (write-string file3 "this is file2")
       (close file1)
       (close file2)
       (close file3))
     (let ((cfile-writer (make-instance 'compound-file-writer
					:directory dir
					:file-name "cfile")))
       (add-file cfile-writer "file1")
       (add-file cfile-writer "file2")
       (add-file cfile-writer "file3")
       (close cfile-writer))
     (let ((cfile-reader (make-instance 'compound-file-reader
					:directory dir
					:file-name "cfile")))
       (let ((file2 (open-input cfile-reader "file2")))
	 (test compound-file-io-buffer-1 (read-string file2) big-string #'equal)
	 (close file2))))))

