(in-package #:montezuma)

(deftestfun test-whitespace-tokenizer
  (with-input-from-string (input "JJWiseman@yahoo.com is My e-mail 52   #$ address. 23#@$")
    (let ((tokenizer (make-instance 'whitespace-tokenizer
				    :input input)))
      (test whitespace-tokenizer-1 (next-token tokenizer) (make-token "JJWiseman@yahoo.com" 0 19) #'token=)
      (test whitespace-tokenizer-2 (next-token tokenizer) (make-token "is" 20 22) #'token=)
      (test whitespace-tokenizer-3 (next-token tokenizer) (make-token "My" 23 25) #'token=)
      (test whitespace-tokenizer-4 (next-token tokenizer) (make-token "e-mail" 26 32) #'token=)
      (test whitespace-tokenizer-5 (next-token tokenizer) (make-token "52" 33 35) #'token=)
      (test whitespace-tokenizer-6 (next-token tokenizer) (make-token "#$" 38 40) #'token=)
      (test whitespace-tokenizer-7 (next-token tokenizer) (make-token "address." 41 49) #'token=)
      (test whitespace-tokenizer-8 (next-token tokenizer) (make-token "23#@$" 50 55) #'token=))))

