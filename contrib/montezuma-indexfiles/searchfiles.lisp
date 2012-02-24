;;; searchfiles.lisp --- search an index

;; Copyright (C) 2008, Yoni Rabkin <yonirabkin@member.fsf.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA

;;; Code:

(in-package #:montezuma-indexfiles)

(defun load-index (index-dir)
  (setq *index* (make-instance 'montezuma:index :path index-dir)))

(defun quicksearch (term &key (showdoc nil))
  (setf term (montezuma::parse
	      (make-instance 'montezuma::query-parser
			     :default-field *default-field*
			     :default-occur 'default-occur)
	      term))
  (let* ((number-of-results 0)
	 (search-result
	  (with-output-to-string (str)
	    (setf number-of-results
		  (montezuma:search-each
		   *index*
		   term
		   #'(lambda (doc score)
		       (format str "~&~A~56T~A~64T~A~%"
			       (pathname-name
				(montezuma:document-value
				 (montezuma:get-document *index* doc) 
				 "path"))
			       doc score)
		       (when showdoc
			 (format str "~&~A~%"
				 (montezuma:document-value
				  (montezuma:get-document *index* doc) 
				  "contents")
				 ))
		       ))))))
    (cond ((string= search-result "")
	   (format t "~&no documents mached the query~%"))
	  (t
	   (format t "filename~56Tdoc #~64Tscore~%")
	   (format t "~A~%" search-result)
	   (format t "~D total matching documents~%" number-of-results)))))

;;; searchfiles.lisp ends here.
