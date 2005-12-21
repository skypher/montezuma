;;; ------------------------------------------------- -*- Mode: LISP -*-

(in-package :asdf)

(defsystem #:montezuma
    :name "Montezuma"
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :licence "MIT"
    :description ""
    :long-description ""

    :depends-on ("cl-ppcre")

    :components
    ((:file "package")
     (:module "util"
	      :components ((:file "porter-stemmer")
			   (:file "streams"))
	      :depends-on ("package"))
     (:module "document"
	      :components ((:file "field")
			   (:file "document"))
	      :depends-on ("package"))
     (:module "analysis"
	      :components ((:file "token")
			   (:file "token-stream")
			   (:file "token-filters" :depends-on ("token" "token-stream"))
			   (:file "tokenizers"    :depends-on ("token" "token-stream")))
	      :depends-on ("package" "util"))))

(defmethod perform ((o test-op) (c (eql (find-system '#:montezuma))))
  (oos 'load-op '#:montezuma-tests)
  (oos 'test-op '#:montezuma-tests :force t))



(defsystem #:montezuma-tests
  :depends-on (#:montezuma)
  :components
  ((:module "tests"
	    :components
	    ((:module "unit"
		      :components
		      ((:file "tests")
		       (:module "document"
				:components ((:file "field")
					     (:file "document"))
				:depends-on ("tests"))
		       (:module "analysis"
				:components ((:file "lowercase-filter")
					     (:file "stop-filter")
					     (:file "porter-stem-filter")
					     (:file "letter-tokenizer")
					     (:file "whitespace-tokenizer")
					     (:file "lowercase-tokenizer"))
				:depends-on ("tests"))))))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system '#:montezuma-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:montezuma)))
      (error "test-op failed")))
