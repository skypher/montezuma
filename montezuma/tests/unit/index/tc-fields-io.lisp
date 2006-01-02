(in-package #:montezuma)

(deftestfixture fields-io
    (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-field-writer
	    (let ((doc (make-instance 'document)))
	      (add-field doc (make-field "name" "daily news" :stored T :index :tokenized))
	      (add-field doc (make-field "content" "Nothing happened today." :stored T))
	      (let ((infos (make-instance 'field-infos)))
		(add-doc-fields infos doc)
		(let ((writer (make-instance 'fields-writer
					     :directory (fixture-var 'dir)
					     :segment "fieldswritertest"
					     :field-infos infos)))
		  (add-document writer doc)
		  (close writer))
		(let ((fstream (open-input (fixture-var 'dir)
					   "fieldswritertest.fdt"))
		      (istream (open-input (fixture-var 'dir)
					   "fieldswritertest.fdx")))
		  (let ((stored (read-vint fstream))
			(field-num1 (read-vint fstream))
			(byte1 (read-byte fstream))
			(data1 (read-string fstream)))
		    (test field-writer-1 stored 2)
		    (test field-writer-2 (and (logbitp +field-is-tokenized-bit+ byte1) T) T)
		    (test field-writer-3 data1 "daily news" #'string=))
		  (let ((field-num2 (read-vint fstream))
			(byte2 (read-byte fstream))
			(data2 (read-string fstream)))
		    (test field-writer-4 (logbitp byte2 +field-is-tokenized-bit+) NIL)
		    (test field-writer-5 data2 "Nothing happened today." #'string=)))))))
