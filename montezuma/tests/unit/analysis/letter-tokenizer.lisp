(in-package #:montezuma)

(deftestfun test-letter-tokenizer
  (with-input-from-string (input "JJwiseman@yahoo.com is My e-mail 523@#$ address. 23#@$")
    (let ((tokenizer (make-instance 'letter-tokenizer :input input)))
      (test letter-tokenizer-1
	    (next-token tokenizer) (make-token "JJwiseman" 0 9)
	    #'token=)
      (test letter-tokenizer-2
	    (next-token tokenizer) (make-token "yahoo" 10 15)
	    #'token=)
      (test letter-tokenizer-3
	    (next-token tokenizer) (make-token "com" 16 19)
	    #'token=)
      (test letter-tokenizer-4
	    (next-token tokenizer) (make-token "is" 20 22)
	    #'token=)
      (test letter-tokenizer-5
	    (next-token tokenizer) (make-token "My" 23 25)
	    #'token=)
      (test letter-tokenizer-6
	    (next-token tokenizer) (make-token "e" 26 27)
	    #'token=)
      (test letter-tokenizer-7
	    (next-token tokenizer) (make-token "mail" 28 32)
	    #'token=)
      (test letter-tokenizer-8
	    (next-token tokenizer) (make-token "address" 40 47)
	    #'token=)
      (test letter-tokenizer-9
	    (next-token tokenizer) nil))))


      