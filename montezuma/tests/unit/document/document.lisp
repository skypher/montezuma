(in-package #:montezuma)

(deftestfun test-document
  (let ((doc (make-document)))
    (let ((f11 (make-field "field1" "value1" :stored T :index NIL))
	  (f12 (make-field "field1" "value2" :stored T :index NIL))
	  (f13 (make-field "field1" "value3" :stored T :index NIL))
	  (f21 (make-field "field2" "value1" :stored T :index NIL)))
      (add-field doc f11)
      (add-field doc f12)
      (add-field doc f13)
      (add-field doc f21)
      (test document-1 (length (document-fields doc "field1")) 3)
      (test document-2 (length (document-fields doc "field2")) 1)
      (let ((field (remove-field doc "field1")))
	(test document-3 (length (document-fields doc "field1")) 2)
	(test document-4 field f11))
      (test document-5 (document-values doc "field1") "value2 value3" #'string=)
      (remove-fields doc "field1")
      (test document-6 (document-field doc "field1") nil))))

      
