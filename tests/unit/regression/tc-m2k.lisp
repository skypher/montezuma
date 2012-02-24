;;; tc-m2k.lisp --- M2K bug (issue 2) regression test
;;
;; Copyright (C) 2009, Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Comments:

;; Problem: after adding 2000 documents to an index he was unable to
;; search without getting an error. Appeared in 0.1.1.

;;; Code:

(in-package #:montezuma)

(defvar *tc-m2k-test-index-path*
  (merge-pathnames
   (make-pathname :directory '(:relative "tc-m2k-test-index"))
   (make-pathname :name nil
		  :type nil
		  :defaults *load-pathname*)))
(defvar *tc-m2k-test-corpus-path*
  (merge-pathnames
   (make-pathname :directory '(:relative "tc-m2k-test-corpus"))
   (make-pathname :name nil
		  :type nil
		  :defaults *load-pathname*)))
(defvar *tc-m2k-test-index* nil)
(defvar *tc-m2k-test-field* "contents")
(defvar *tc-m2k-test-verbose* nil)
(defvar *tc-m2k-test-words*
  '("no" "Freeman" "shall" "be" "taken" "or" "imprisoned" "disseised"
    "of" "his" "Freehold"  "Liberties" "free" "Customs" "outlawed"
    "exiled" "any" "other" "wise" "destroyed" "nor" "will" "We" "not"
    "pass" "upon" "him" "nor" "condemn" "him" "but" "by" "lawful"
    "judgment" "of" "his" "Peers"  "by" "the" "Law" "of" "the" "Land"
    "We" "will" "sell" "to" "no" "man" "we" "will" "not" "deny"
    "defer" "to" "any" "man" "either" "Justice"  "Right" "
"))
(defvar *tc-m2k-test-term* "free")
(defparameter *tc-m2k-test-number-of-files* 2000)

;;; utility functions

(defun tc-m2k-defcounter ()
  (let ((n 0))
    (lambda () (incf n))))

(defun tc-m2k-make-content ()
  (flet ((mod-shuffle (l)
	   (loop for j from (1- (length l)) above 0 do
		(rotatef (nth (random (length l)) l) (nth j l)))
	   l))
    (apply 'concatenate 'string
	   (loop for i from 1 upto (1+ (random 9))
	      append
		(loop for w in (mod-shuffle *tc-m2k-test-words*)
		   for i from 0
		   when (= (random 10) 0)
		   append (list w " "))))))

;;; functions for writing a test corpus unto disk, indexing and
;;; searching it

(defun tc-m2k-write-test-file (counter path)
  "Write a test file with generated content to disk."
  (let ((filename (concatenate 'string "test"
			       (format nil "~d" (funcall counter))))
	(contents (tc-m2k-make-content)))
    (ensure-directories-exist (make-pathname :defaults path))
    (with-open-file
	(out (merge-pathnames
	      (make-pathname :defaults *tc-m2k-test-corpus-path*)
	      (make-pathname :name     filename
			     :type     "txt"))
	     :direction    :output
	     :if-exists    :supersede
	     :element-type '(unsigned-byte 8))
      (loop for int across contents do
	   (cl:write-byte (char-code int) out)))))

(defun tc-m2k-write-test-files (path)
  "Write `*tc-m2k-test-number-of-files*' test files to disk."
  (let ((c (tc-m2k-defcounter)))
    (dotimes (i *tc-m2k-test-number-of-files*)
      (tc-m2k-write-test-file c path)
      (when *tc-m2k-test-verbose* (format t "w")))
    (when *tc-m2k-test-verbose* (format t "~%"))))

(defun tc-m2k-index-docs-from-disk (corpus-path index-path)
  "Index test files."
  (cl-fad:delete-directory-and-files index-path 
				     :if-does-not-exist :ignore)
  (let ((filelist (cl-fad:list-directory corpus-path))
	(index (make-instance 'montezuma::index :path index-path)))
    (dolist (corpus-path filelist)
      (handler-case
	  (let ((this (make-instance 'document)))
	    ;; contents: index and tokenize, but don't store
	    (add-field
	     this
	     (make-field
	      *tc-m2k-test-field*
	      (with-open-file
		  (stream corpus-path)
		(let ((seq
		       (make-array (cl:file-length stream)
				   :element-type 'character
				   :fill-pointer t)))
		  (setf (fill-pointer seq)
			(read-sequence seq stream))
		  seq))
	      ;; doesn't work because Montezuma is buggy
	      ;; :store-term-vector :with-positions-offsets
	      :stored nil
	      :index :tokenized))
	    ;; finally, add the document
	    (add-document-to-index index this)
	    (when *tc-m2k-test-verbose* (format t "i")))))
    (when *tc-m2k-test-verbose* (format t "~%"))
    (cl-fad:delete-directory-and-files corpus-path
				       :if-does-not-exist :ignore)
    index))

(defun tc-m2k-write-index-search ()
  "Write test files, index them and perform a search."
  (tc-m2k-write-test-files *tc-m2k-test-corpus-path*)
  (when *tc-m2k-test-verbose*
    (format t "~&searching..~%"))
  (search-each
   (tc-m2k-index-docs-from-disk 
    *tc-m2k-test-corpus-path*
    *tc-m2k-test-index-path*)
   (montezuma::parse
    (make-instance
     'montezuma::query-parser
     :default-field *tc-m2k-test-field*
     :default-occur 'default-occur)
    *tc-m2k-test-term*)
   #'(lambda (doc score)
       (when *tc-m2k-test-verbose* (format t "~&doc ~S, score ~S~%" doc score))))
  t)

(deftestfun test-m2k-issue
  (test test-m2k-issue 
	(tc-m2k-write-index-search) t))

;;; tc-m2k.lisp ends here.
