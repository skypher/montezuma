(in-package #:montezuma)

(deftestfun test-stop-analyzer
  (with-input-from-string (input "The Quick AND the DEAD the and to it there their")
    (let* ((analyzer (make-instance 'stop-analyzer))
	   (token-stream (token-stream analyzer "field" input)))
      (test stop-analyzer-1 (next-token token-stream) (make-token "quick" 4 9) #'token=)
      (test stop-analyzer-2 (next-token token-stream) (make-token "dead" 18 22) #'token=)
      (test stop-analyzer-3 (next-token token-stream) nil)))
  (with-input-from-string (input "john wiseman")
    (let* ((analyzer (make-instance 'stop-analyzer :stop-words '("john")))
	   (token-stream (token-stream analyzer "field" input)))
      (test stop-analyzer-4 (next-token token-stream) (make-token "wiseman" 5 12) #'token=)
      (test stop-analyzer-5 (next-token token-stream) nil))))
