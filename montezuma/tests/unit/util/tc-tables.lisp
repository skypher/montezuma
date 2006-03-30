(in-package #:montezuma)


(defun do-test-table (table)
  (atest table-1 (length (table-values table)) 0)
  (atest table-2 (length (table-keys table)) 0)
  (atest table-3 (table-value table :foo 7) 7)
  (setf (table-value table :foo) 8)
  (atest table-4 (table-value table :foo 7) 8)
  (atest table-5 (table-keys table) '(:foo) #'set=)
  (atest table-6 (table-values table) '(8) #'set=)
  (atest table-7 (and (in-table-p table :foo) T) T)
  (setf (table-value table :bar) T)
  (atest table-8 (table-value table :foo) 8)
  (atest table-9 (table-value table :bar) T)
  (atest table-10 (table-keys table) '(:foo :bar) #'set=)
  (atest table-11 (table-values table) '(8 T) #'set=)
  (atest table-12 (and (in-table-p table :foo) T) T)
  (atest table-13 (and (in-table-p table :bar) T) T)
  (remtable table :foo)
  (atest table-14 (in-table-p table :foo) NIL)
  (clrtable table)
  (atest table-15 (length (table-keys table)) 0)
  (atest table-16 (length (table-values table)) 0))

(deftestfun test-tables
  (flet ((set= (a b) (and (null (set-difference a b)) (null (set-difference b a)))))
    ;; Riesbeck-style tables
    (let ((table (make-table :test #'eql)))
      (do-test-table table)
    (let ((table (make-table :test #'equal)))
      (setf (table-value table "age") 35)
      (test table-17 (and (in-table-p table "age") T) T))
    ;; hash tables
    (let ((table (make-hash-table)))
      (do-test-table table))
    ;; Association lists
    (let ((table (convert-alist-to-table '((:foo . 8) (:bar . T)))))
      (test table-1 (length (table-values table)) 2)
      (atest table-2 (length (table-keys table)) 2)
      (atest table-3 (table-value table :foo 7) 8)
      (atest table-7 (and (in-table-p table :foo) T) T)
      (atest table-9 (table-value table :bar) T)
      (atest table-10 (table-keys table) '(:foo :bar) #'set=)
      (atest table-11 (table-values table) '(8 T) #'set=)
      (atest table-12 (and (in-table-p table :foo) T) T)
      (atest table-13 (and (in-table-p table :bar) T) T)))))

      
