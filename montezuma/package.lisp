(cl:defpackage #:montezuma
  (:shadow #:directory #:read-byte #:write-byte #:write-string 
	   #:close #:delete-file #:rename-file
	   #:file-length #:read #:write #:delete)
  (:use #:common-lisp))
