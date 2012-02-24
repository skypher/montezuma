;;; indexfiles.lisp --- create an index from a directory tree

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

(defun accept-docs-by-ext (filename)
  "Filter file names by extension."
  (when (or (member (pathname-type filename) *accept-ext-list* :test #'string=)
	    (cl-fad:directory-pathname-p filename))
    filename))

(defun index-docs (dir)
  "Recursively index all files under DIR."
  (when (not (cl-fad:directory-exists-p dir))
    (error "can't read directory: ~A" dir))
  (when (not *index*)
    (error "no current index"))
  (let ((filelist
	 (remove-if-not *accept-function*
			(cl-fad:list-directory dir))))
    (dolist (path filelist)
      (cond ((cl-fad:directory-pathname-p path)
	     (index-docs path))
	    ((cl-fad:file-exists-p path)
	     (format t "adding ~A~%" path)
	     (handler-case
		 (let ((this (make-instance 'montezuma:document)))

		   ;; path: index and store, but don't tokenize
		   (montezuma:add-field
		    this
		    (montezuma:make-field
		     "path"
		     (format nil "~A" path)
		     :index :untokenized))

		   ;; contents: index and tokenize, but don't store
		   (montezuma:add-field
		    this
		    (montezuma:make-field
		     "contents"
		     (with-open-file
			 (stream path)
		       (let ((seq (make-array (file-length stream)
					      :element-type 'character
					      :fill-pointer t)))
			 (setf (fill-pointer seq)
			       (read-sequence seq stream))
			 seq))
		     :index :tokenized :stored nil))

		   ;; finally, add the document
		   (montezuma:add-document-to-index *index* this))
	       (sb-int:stream-decoding-error ()
		 (format t "decoding error, skipping file~%")))))))
  ;; 27/10/2008: optimizing is a workaround to a bug which makes
  ;; montezuma die
  (montezuma:optimize *index*))

(defun index-directory-tree (tree-dir &key
			     (index-dir nil) (ram nil)
			     (merge-factor 10) (min-merge-docs 10))
  (when (cl-fad:directory-exists-p index-dir)
    (error "Cannot save index to ~A directory, please delete it first" index-dir))
  (let ((montezuma::*index-writer-default-merge-factor* merge-factor)
	(montezuma::*index-writer-default-min-merge-docs* min-merge-docs))
    (sb-posix:mkdir index-dir *default-directory-permissions*)
    (setf *index*
	  (if ram
	      (make-instance 'montezuma:index)
	      (make-instance 'montezuma:index :path index-dir)))
    (format t "Indexing to directory '~A'..." index-dir)
    (index-docs tree-dir)))

;;; indexfiles.lisp ends here.
