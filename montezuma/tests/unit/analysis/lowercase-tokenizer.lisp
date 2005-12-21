(in-package #:montezuma)

(deftestfun test-lowercase-tokenizer-normalize
  (let ((lt (make-instance 'lowercase-tokenizer
			   :input "")))
    (test lowercase-tokenizer-normalize-1 (normalize lt "!") "!" #'string=)
    (test lowercase-tokenizer-normalize-2 (normalize lt "r") "r" #'string=)
    (test lowercase-tokenizer-normalize-3 (normalize lt "R") "r" #'string=)))

(deftestfun test-lowercase-tokenizer
  (with-input-from-string (input "JJWiseman@yahoo.com is My E-Mail 523@#$ ADDRESS. 23#@$")
    (let ((lt (make-instance 'lowercase-tokenizer
			     :input input)))
      (test lowercase-tokenizer-1 (next-token lt) (make-token "jjwiseman" 0 9) #'token=)
      (test lowercase-tokenizer-2 (next-token lt) (make-token "yahoo" 10 15) #'token=)
      (test lowercase-tokenizer-3 (next-token lt) (make-token "com" 16 19) #'token=)
      (test lowercase-tokenizer-4 (next-token lt) (make-token "is" 20 22) #'token=)
      (test lowercase-tokenizer-5 (next-token lt) (make-token "my" 23 25) #'token=)
      (test lowercase-tokenizer-6 (next-token lt) (make-token "e" 26 27) #'token=)
      (test lowercase-tokenizer-7 (next-token lt) (make-token "mail" 28 32) #'token=)
      (test lowercase-tokenizer-8 (next-token lt) (make-token "address" 40 47) #'token=)
      (test lowercase-tokenizer-9 (next-token lt) nil))))


      
