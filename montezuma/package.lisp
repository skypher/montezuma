(cl:defpackage #:montezuma
  (:shadow #:read-byte #:write-byte #:write-string 
	   #:close #:delete-file #:rename-file
	   #:file-length)
  (:use #:common-lisp))
