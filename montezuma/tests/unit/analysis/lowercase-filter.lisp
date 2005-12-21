(in-package #:montezuma)

(deftestfun test-lowercase-filter
  (with-input-from-string (input "JJwiseman@yahoo.com is My E-Mail 52     #$ ADDRESS. 23#@$")
    (let ((filter (make-instance 'lowercase-filter
				 :input (make-instance 'whitespace-tokenizer
						       :input input))))
      (test lowercase-filter-1
	    (next-token filter) (make-token "jjwiseman@yahoo.com" 0 19)
	    #'token=)
      (test lowercase-filter-2
	    (next-token filter) (make-token "is" 20 22)
	    #'token=)
      (test lowercase-filter-3
	    (next-token filter) (make-token "my" 23 25)
	    #'token=)
      (test lowercase-filter-4
	    (next-token filter) (make-token "e-mail" 26 32)
	    #'token=)
      (test lowercase-filter-5
	    (next-token filter) (make-token "52" 33 35)
	    #'token=)
      (test lowercase-filter-6
	    (next-token filter) (make-token "#$" 40 42)
	    #'token=)
      (test lowercase-filter-7
	    (next-token filter) (make-token "address." 43 51)
	    #'token=)
      (test lowercase-filter-8
	    (next-token filter) (make-token "23#@$" 52 57)
	    #'token=)
      (test lowercase-filter-9
	    (next-token filter) nil))))
