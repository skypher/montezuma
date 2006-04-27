(in-package #:common-lisp-user)

(defpackage #:montezuma-lift
  (:shadow #:directory #:read-byte #:write-byte #:write-string 
	   #:delete-file #:rename-file #:count
	   #:merge #:file-length #:read #:write #:delete)
  (:shadowing-import-from #:montezuma 
                          #:optimize #:close #:search)
  (:use #:common-lisp #:montezuma #:lift))