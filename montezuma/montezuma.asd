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

    :depends-on ("cl-ppcre" "cl-fad")

    :components
    ((:file "package")
     (:module "util"
	      :components ((:file "while")
			   (:file "porter-stemmer")
			   (:file "streams")
			   (:file "mop")
			   (:file "priority-queue" :depends-on ("while"))
			   (:file "strings"))
	      :depends-on ("package"))
     (:module "store"
	      :components ((:file "directory")
			   (:file "index-io")
			   (:file "buffered-index-io"
				  :depends-on ("index-io"))
			   (:file "ram-store"
				  :depends-on ("buffered-index-io" "directory"))
			   (:file "fs-store"
				  :depends-on ("buffered-index-io" "directory")))
	      :depends-on ("package" "util"))
     (:module "document"
	      :components ((:file "field")
			   (:file "document"))
	      :depends-on ("package"))
     (:module "analysis"
	      :components ((:file "token")
			   (:file "token-stream")
			   (:file "token-filters" :depends-on ("token" "token-stream"))
			   (:file "tokenizers"    :depends-on ("token" "token-stream"))
			   (:file "standard-tokenizer" :depends-on ("tokenizers"))
			   (:file "analyzers" :depends-on ("standard-tokenizer")))
	      :depends-on ("package" "util"))
     (:module "index"
	      :components ((:file "index-filenames")
			   (:file "term")
			   (:file "term-info")
			   (:file "term-buffer" :depends-on ("term"))
			   (:file "field-infos")
			   (:file "term-enum")
			   (:file "term-doc-enum")
			   (:file "term-infos-io")
			   (:file "multiple-term-doc-pos-enum")
			   (:file "term-vector-offset-info")
			   (:file "segment-term-enum" :depends-on ("term-infos-io")))
	      :depends-on ("analysis"))))

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
		       (:module "util"
				:components ((:file "priority-queue"))
				:depends-on ("tests"))
		       (:module "store"
				:components ((:file "store")
					     (:file "ram-store"
						    :depends-on ("store"))
					     (:file "fs-store"
						    :depends-on ("store")))
				:depends-on ("tests"))
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
					     (:file "lowercase-tokenizer")
					     (:file "standard-tokenizer")
					     (:file "analyzer")
					     (:file "stop-analyzer")
					     (:file "whitespace-analyzer")
					     (:file "standard-analyzer"))
				:depends-on ("tests"))
		       (:module "index"
				:components ((:file "term")
					     (:file "term-info")
					     (:file "term-buffer")
					     (:file "field-infos")
					     (:file "term-infos-io"))
				:depends-on ("tests"))))))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system '#:montezuma-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:montezuma)))
      (error "test-op failed")))
