;;; ------------------------------------------------- -*- Mode: LISP -*-

(in-package :asdf)

(defsystem #:montezuma
    :name "Montezuma"
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :licence "MIT"
    :description "Montezuma is a port of the Lucene text search engine library."
    :long-description ""

    :depends-on ("cl-ppcre" "cl-fad" "metacopy")

    :components
    ((:file "package")
     (:module "util"
       :components ((:file "while")
		    (:file "porter-stemmer")
		    (:file "streams")
		    (:file "pipes")
		    (:file "mop")
		    (:file "priority-queue"      :depends-on ("while"))
		    (:file "strings")
		    (:file "tables")
		    (:file "bit-vector")
		    (:file "files")
                    (:file "comparable")
                    (:file "utilities"))
       :depends-on ("package"))
     (:module "store"
       :components ((:file "directory")
		    (:file "index-io")
		    (:file "buffered-index-io"   :depends-on ("index-io"))
		    (:file "ram-store"           :depends-on ("buffered-index-io" "directory"))
		    (:file "fs-store"            :depends-on ("buffered-index-io" "directory")))
       :depends-on ("package" "util"))
     (:module "document"
       :components ((:file "field")
		    (:file "document"))
       :depends-on ("package"))
     (:module "analysis"
       :components ((:file "token")
		    (:file "token-stream")
		    (:file "token-filters"       :depends-on ("token" "token-stream"))
		    (:file "tokenizers"          :depends-on ("token" "token-stream"))
		    (:file "standard-tokenizer"  :depends-on ("tokenizers"))
		    (:file "analyzers"           :depends-on ("standard-tokenizer")))
       :depends-on ("package" "util"))
     (:module "search"
              :components ((:file "api")
                           (:file "similarity")
                           (:file "boolean-clause")
                           (:file "scorer")
                           (:file "score-doc")
                           (:file "score-doc-comparator")
                           (:file "weight")
                           (:file "query")
                           (:file "term-query"))
              :depends-on ("package"))
     (:module "index"
       :components ((:file "index-filenames")
		    (:file "term")
		    (:file "term-info")
		    (:file "term-buffer"         :depends-on ("term"))
		    (:file "field-infos")
		    (:file "term-enum")
		    (:file "term-doc-enum")
		    (:file "term-infos-io")
		    (:file "multiple-term-doc-pos-enum")
		    (:file "term-vector-offset-info")
		    (:file "segment-term-vector")
		    (:file "term-vectors-io"     :depends-on ("segment-term-vector"))
		    (:file "segment-term-enum"   :depends-on ("term-infos-io"))
		    (:file "fields-io")
		    (:file "compound-file-io")
		    (:file "segment-merge-info")
		    (:file "segment-merge-queue")
		    (:file "segment-infos")
		    (:file "segment-reader"      :depends-on ("index-reader"))
		    (:file "multi-reader"        :depends-on ("index-reader"))
		    (:file "index-writer"        :depends-on ("segment-reader"))
		    (:file "document-writer")
		    (:file "segment-merger"      :depends-on ("index-filenames" "fields-io" "term-buffer"))
		    (:file "index-reader")
		    (:file "index"))
       :depends-on ("analysis" "search"))))

(defmethod perform ((o test-op) (c (eql (find-system '#:montezuma))))
  (oos 'load-op '#:montezuma-tests)
  (oos 'test-op '#:montezuma-tests :force t))



(defsystem #:montezuma-tests
  :depends-on (#:montezuma)
  :components
  ((:module "tests"
     :components
      ((:module "unit"
	  :components ((:file "tests")
		       (:module "util"
			 :components ((:file "tc-priority-queue")
				      (:file "tc-tables"))
			 :depends-on ("tests"))
		       (:module "store"
			 :components ((:file "tc-store")
				      (:file "tc-ram-store")
				      (:file "tc-fs-store"))
			 :depends-on ("tests"))
		       (:module "document"
			 :components ((:file "tc-field")
				      (:file "tc-document"))
			 :depends-on ("tests"))
		       (:module "analysis"
			 :components ((:file "tc-lowercase-filter")
				      (:file "tc-stop-filter")
				      (:file "tc-porter-stem-filter")
				      (:file "tc-letter-tokenizer")
				      (:file "tc-whitespace-tokenizer")
				      (:file "tc-lowercase-tokenizer")
				      (:file "tc-standard-tokenizer")
				      (:file "tc-analyzer")
				      (:file "tc-stop-analyzer")
				      (:file "tc-whitespace-analyzer")
				      (:file "tc-standard-analyzer"))
			 :depends-on ("tests"))
                       (:module "search"
                         :components ((:file "tc-similarity"))
                         :depends-on ("tests"))
		       (:module "index"
			 :components ((:file "tc-term")
				      (:file "tc-term-info")
				      (:file "tc-term-buffer")
				      (:file "tc-field-infos")
				      (:file "tc-term-infos-io")
				      (:file "tc-term-vectors-io")
				      (:file "tc-fields-io")
				      (:file "tc-compound-file-io")
				      (:file "tc-segment-term-enum")
				      (:file "tc-segment-term-vector")
				      (:file "tc-segment-infos")
				      (:file "th-doc")
				      (:file "tc-index-writer" :depends-on ("th-doc"))
				      (:file "tc-index-reader" :depends-on ("th-doc"))
				      (:file "tc-multiple-term-doc-pos-enum" :depends-on ("th-doc")))
			 :depends-on ("tests"))))))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system '#:montezuma-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:montezuma)))
      (error "test-op failed")))
