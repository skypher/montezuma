(in-package #:montezuma)

(deftestfun test-porter-stem-filter
  (with-input-from-string (input "breath Breathes BreatHed BREATHING")
    (let ((tokenizer (make-instance 'porter-stem-filter
		        :input (make-instance 'lowercase-filter
				  :input (make-instance 'whitespace-tokenizer
					    :input input)))))
      (test porter-stem-filter-1 (next-token tokenizer) (make-token "breath" 0 6) #'token=)
      (test porter-stem-filter-2 (next-token tokenizer) (make-token "breath" 7 15) #'token=)
      (test porter-stem-filter-3 (next-token tokenizer) (make-token "breath" 16 24) #'token=)
      (test porter-stem-filter-4 (next-token tokenizer) (make-token "breath" 25 34) #'token=))))

      
	   