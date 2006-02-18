(cl:defpackage #:montezuma
  (:shadow #:directory #:read-byte #:write-byte #:write-string 
	   #:close #:delete-file #:rename-file #:count
	   #:merge #:file-length #:read #:write #:delete #:optimize)
  (:use #:common-lisp))
