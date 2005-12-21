(in-package #:montezuma)

(deftestfun test-standard-tokenizer
  (with-input-from-string (input "JJWiseman@yahoo.com is My e-mail 523@#$ address. 23#@$")
    (let ((tokenizer (make-instance 'standard-tokenizer
				    :input input)))
      (test standard-tokenizer-1 (next-token tokenizer) (make-token "JJWiseman@yahoo.com" 0 19) #'token=)
      (test standard-tokenizer-2 (next-token tokenizer) (make-token "is" 20 22) #'token=)
      (test standard-tokenizer-3 (next-token tokenizer) (make-token "My" 23 25) #'token=)
      (test standard-tokenizer-4 (next-token tokenizer) (make-token "e" 26 27) #'token=)
      (test standard-tokenizer-5 (next-token tokenizer) (make-token "mail" 28 32) #'token=)
      (test standard-tokenizer-6 (next-token tokenizer) (make-token "523" 33 36) #'token=)
      (test standard-tokenizer-7 (next-token tokenizer) (make-token "address" 40 47) #'token=)
      (test standard-tokenizer-8 (next-token tokenizer) (make-token "23" 49 51) #'token=)
      (test standard-tokenizer-9 (next-token tokenizer) nil))))
