(in-package #:montezuma)

(deftestfun test-whitespace-analyzer
  (with-input-from-string (input "JJWiseman@yahoo.com is My e-mail 52   #$ address. 23#@$")
    (let* ((analyzer (make-instance 'whitespace-analyzer))
	   (token-stream (token-stream analyzer "field" input)))
      (test whitespace-analyzer-1 (next-token token-stream) (make-token "JJWiseman@yahoo.com" 0 19) #'token=)
      (test whitespace-analyzer-2 (next-token token-stream) (make-token "is" 20 22) #'token=)
      (test whitespace-analyzer-3 (next-token token-stream) (make-token "My" 23 25) #'token=)
      (test whitespace-analyzer-4 (next-token token-stream) (make-token "e-mail" 26 32) #'token=)
      (test whitespace-analyzer-5 (next-token token-stream) (make-token "52" 33 35) #'token=)
      (test whitespace-analyzer-6 (next-token token-stream) (make-token "#$" 38 40) #'token=)
      (test whitespace-analyzer-7 (next-token token-stream) (make-token "address." 41 49) #'token=)
      (test whitespace-analyzer-8 (next-token token-stream) (make-token "23#@$" 50 55) #'token=)
      (test whitespace-analyzer-9 (next-token token-stream) nil))))

