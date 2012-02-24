;;; -*- lisp -*-
;;; montezuma-indexfiles.asd --- System definition for montezuma-indexfiles

;;; Copyright 2008 Yoni Rabkin Katzenell Under the GNU General Public
;;; Licence version 3 or later.

;;; Code:

(defpackage #:montezuma-indexfiles-system
  (:use #:common-lisp
	#:asdf))

(in-package #:montezuma-indexfiles-system)

(defsystem "montezuma-indexfiles"
  :description "montezuma-indexfiles"
  :author "Yoni Rabkin <yonirabkin@member.fsf.org>"
  :licence "GNU General Public License"
  :serial t
  :depends-on (#:cl-fad
	       #:montezuma)
  :components ((:file "packages")
	       (:file "specials")
	       (:file "searchfiles")
	       (:file "indexfiles")))

;;; montezuma-indexfiles.asd ends here.
