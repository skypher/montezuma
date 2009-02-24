;;; specials.lisp --- special variables

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

(defparameter *default-directory-permissions* #o755)
(defparameter *index* nil)
(defparameter *accept-ext-list* nil)
(defparameter *accept-function* #'(lambda (x) (declare (ignore x)) t))
(defparameter *default-field* "contents")

;;; specials.lisp ends here.
