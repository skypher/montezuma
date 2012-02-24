(in-package #:montezuma)

(deftestfun test-string-to-bytes
  (test string-to-bytes-1
        (length (string-to-bytes "foo" :start 0 :end 3)) 3)
  (test string-to-bytes-2
        (length (string-to-bytes "foo" :start 2 :end 3)) 1))

