(in-package #:montezuma)

(deftestfun test-field-info
  (let ((fi (make-instance 'field-info
			   :name "name"
			   :indexed-p T
			   :number 1
			   :store-term-vector-p T
			   :store-positions-p T
			   :store-offsets-p T
			   :omit-norms-p T)))
    (test field-info-1 (field-name fi) "name" #'string=)
    (test field-info-2 (field-number fi) 1)
    (test field-info-3 (and (field-indexed-p fi) T) T)
    (test field-info-4 (and (field-store-term-vector-p fi) T) T)
    (test field-info-5 (and (field-store-offsets-p fi) T) T)
    (test field-info-6 (and (field-store-positions-p fi) T) T)
    (test field-info-7 (and (field-omit-norms-p fi) T) T)
    (setf (field-name fi) "hello"
	  (field-indexed-p fi) NIL
	  (field-number fi) 2
	  (field-store-term-vector-p fi) NIL
	  (field-store-offsets-p fi) NIL
	  (field-store-positions-p fi) NIL
	  (field-omit-norms-p fi) NIL)
    (test field-info-8 (field-name fi) "hello" #'string=)
    (test field-info-9 (field-number fi) 2)
    (test field-info-10 (field-indexed-p fi) NIL)
    (test field-info-11 (field-store-term-vector-p fi) NIL)
    (test field-info-12 (field-store-offsets-p fi) NIL)
    (test field-info-13 (field-store-positions-p fi) NIL)
    (test field-info-14 (field-omit-norms-p fi) NIL)
    (let ((fi-2 (make-instance 'field-info
			       :name "name"
			       :indexed-p T
			       :number 1
			       :store-term-vector-p T)))
      (test field-info-15 (field-store-offsets-p fi-2) NIL)
      (test field-info-16 (field-store-positions-p fi-2) NIL)
      (test field-info-17 (field-omit-norms-p fi-2) NIL))))


(defun do-test-field-info-attrs (fi name number indexed-p store-term-vector-p store-positions-p
			      store-offsets-p omit-norms-p)
  (atest test-field-info-attr (field-name fi) name #'string=)
  (atest test-field-info-attr (field-number fi) number)
  (atest test-field-info-attr (and (field-indexed-p fi) T) indexed-p)
  (atest test-field-info-attr (and (field-store-term-vector-p fi) T) store-term-vector-p)
  (atest test-field-info-attr (and (field-store-positions-p fi) T) store-positions-p)
  (atest test-field-info-attr (and (field-store-offsets-p fi) T) store-offsets-p)
  (atest test-field-info-attr (and (field-omit-norms-p fi) T) omit-norms-p))

(deftestfun test-field-infos-add
  (let ((fis (make-instance 'field-infos)))
    (let ((fi (add-field-info fis "field1" :indexed-p NIL)))
      (do-test-field-info-attrs fi "field1" 0 NIL NIL NIL NIL NIL)
      (test field-infos-add-1 (size fis) 1))
    (let ((fi (add-field-info fis "field1" :indexed-p T :store-term-vector T)))
      (do-test-field-info-attrs fi "field1" 0 T T NIL NIL NIL)
      (test field-infos-add-3 (size fis) 1))
    (let ((fi (add-field-info fis "field2" :indexed-p NIL)))
      (do-test-field-info-attrs fi "field2" 1 NIL NIL NIL NIL NIL)
      (test field-infos-add-4 (size fis) 2))))

(deftestfun test-field-infos-has-vectors
  (let ((fis (make-instance 'field-infos)))
    (test field-infos-has-vectors-1 (has-vectors-p fis) NIL)
    (add-field-info fis "random field")
    (test field-infos-has-vectors-2 (has-vectors-p fis) NIL)
    (add-field-info fis "store-term-vector-field" :indexed-p T :store-term-vector T :store-position NIL
		    :store-offset NIL :omit-norms NIL)
    (test field-infos-has-vectors-3 (and (has-vectors-p fis) T) T)))

(deftestfun test-field-infos-rw
  (let ((dir (make-instance 'ram-directory)))
    (let ((fis (make-instance 'field-infos)))
      (add-field-info fis "field1"
		      :indexed-p NIL :store-term-vector NIL :store-position NIL :store-offset NIL
		      :omit-norms T)
      (add-field-info fis "field2"
		      :indexed-p T :store-term-vector NIL :store-position NIL :store-offset NIL
		      :omit-norms T)
      (add-field-info fis "field3"
		      :indexed-p T :store-term-vector T :store-position NIL :store-offset NIL
		      :omit-norms T)
      (add-field-info fis "field4"
		      :indexed-p T :store-term-vector T :store-position T :store-offset NIL
		      :omit-norms T)
      (add-field-info fis "field5"
		      :indexed-p T :store-term-vector T :store-position T :store-offset T
		      :omit-norms T)
      (add-field-info fis "field6"
		      :indexed-p T :store-term-vector T :store-position T :store-offset T
		      :omit-norms NIL)
      (write-to-dir fis dir "fis_rw.test"))
    (let ((fis (make-instance 'field-infos
			      :dir dir :name "fis_rw.test")))
      (do-test-field-info-attrs (get-field fis 0) "field1" 0 NIL NIL NIL NIL T)
      (do-test-field-info-attrs (get-field fis 1) "field2" 1 T NIL NIL NIL T)
      (do-test-field-info-attrs (get-field fis 2) "field3" 2 T T NIL NIL T)
      (do-test-field-info-attrs (get-field fis 3) "field4" 3 T T T NIL T)
      (do-test-field-info-attrs (get-field fis 4) "field5" 4 T T T T T)
      (do-test-field-info-attrs (get-field fis 5) "field6" 5 T T T T NIL)
      (test field-infos-rw-1 (size fis) 6))))



    