(in-package #:common-lisp-user)
(defpackage #:asdf-lucene-in-action-tests (:use #:asdf #:cl))
(in-package #:asdf-lucene-in-action-tests)

(defsystem lucene-in-action-tests
  :components ((:file "package")
               (:file "utility" :depends-on ("package"))
               ;(:file "listing-2-1" :depends-on ("utility"))
               (:file "jotting" :depends-on ("utility")))
  :depends-on (montezuma lift))
