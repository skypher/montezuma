;; I feel bad about all this shadowing.  It's definitely on the list
;; of things to revisit.

(cl:defpackage #:montezuma
  (:shadow #:directory #:read-byte #:write-byte #:write-string 
	   #:close #:delete-file #:rename-file #:count
	   #:merge #:file-length #:read #:write #:delete #:optimize)
  (:use #:common-lisp))
