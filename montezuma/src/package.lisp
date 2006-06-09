(in-package #:common-lisp)

(defpackage #:montezuma.parser
  (:use #:cl)
  (:shadow #:type)
  (:export
   #:defprod
   #:defchartype
   #:deflexer
   #:defparser
   #:parselet
   #:?
   #:*
   #:+
   #:~
   #:!
   #:/
   #:&
   #:@
   #:^
   #:%))


;; I feel bad about all this shadowing.  It's definitely on the list
;; of things to revisit.

(defpackage #:montezuma
  (:shadow #:directory #:read-byte #:write-byte #:write-string 
	   #:close #:delete-file #:rename-file #:count #:search
	   #:merge #:file-length #:read #:write #:delete #:optimize)
  (:use #:common-lisp #:montezuma.parser)
  (:export 
   #:make-fs-directory
   #:standard-analyzer
   #:index-writer
   #:document
   #:add-field
   #:add-document
   #:add-document-to-index
   #:add-document-to-index-writer
   #:optimize
   #:close
   #:field
   #:search
   #:index
   #:writer
   #:optimize
   #:make-term
   #:max-doc
   #:search
   #:weight
   #:scorer
   #:reader 
   #:each-hit
   #:each
   #:doc
   #:score
   #:size
   #:get-document
   
   #:term-query
   #:index-searcher))
