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
	   #:merge #:file-length #:read #:write #:delete #:optimize
	   #:sort)
  (:use #:common-lisp #:montezuma.parser)
  (:export 
   #:make-fs-directory
   #:standard-analyzer
   #:index-writer
   #:document
   #:add-field
   #:make-field
   #:add-document
   #:add-document-to-index
   #:add-document-to-index-writer
   #:search-each
   #:optimize
   #:close
   #:field
   #:field-data
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
   #:document-field
   #:term-query
   #:index-searcher
   
   #:commit
   #:flush
   #:delete

   #:token-image
   #:all-tokens

   #:analyzer

   #:boolean-query
   #:boolean-clause
   #:add-query))
