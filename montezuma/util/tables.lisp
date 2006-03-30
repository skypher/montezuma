(in-package #:montezuma)


(defgeneric table-value (table key &optional value))
(defgeneric set-table-value (table key value))
(defgeneric (setf table-value) (value table key))
(defgeneric in-table-p (table key))
(defgeneric table-values (table))
(defgeneric table-keys (table))
(defgeneric remtable (table key))
(defgeneric clrtable (table))
(defgeneric table-like-p (thing))

(defmethod (setf table-value) (value table key)
  (set-table-value table key value))

(defmethod table-like-p (thing)
  NIL)

;; Hash tables

(defmethod table-like-p ((table hash-table))
  T)

(defmethod table-value ((table hash-table) key &optional value)
  (gethash key table value))

(defmethod set-table-value ((table hash-table) key value)
  (setf (gethash key table) value))

(defmethod in-table-p ((table hash-table) key)
  (multiple-value-bind (value present-p)
      (gethash key table)
    (declare (ignore value))
    present-p))

(defmethod table-values ((table hash-table))
  (loop for value being the hash-value of table
       collect value))

(defmethod table-keys ((table hash-table))
  (loop for key being the hash-key of table
       collect key))

(defmethod remtable ((table hash-table) key)
  (remhash key table))

(defmethod clrtable ((table hash-table))
  (clrhash table))


;; This is basically Chris Riesbeck's implementation.

(defstruct table
  entries
  test)

(defmethod table-like-p ((table table))
  T)

(defmethod table-entry ((table table) key)
  (assoc key (table-entries table) :test (table-test table)))

(defmethod table-value ((table table) key &optional value)
  (let ((entry (table-entry table key)))
    (if entry
	(cdr entry)
	value)))

(defmethod set-table-value ((table table) key value)
  (let ((entry (table-entry table key)))
    (if entry
	(setf (cdr entry) value)
	(setf (table-entries table)
	      (append (table-entries table)
		      (list (cons key value)))))
    value))

(defmethod in-table-p ((table table) key)
  (not (null (table-entry table key))))

(defmethod table-values ((table table))
  (mapcar #'cdr (table-entries table)))

(defmethod table-keys ((table table))
  (mapcar #'car (table-entries table)))

(defmethod remtable ((table table) key)
  (let ((entry (table-entry table key)))
    (when entry
      (setf (table-entries table) (cl:delete entry (table-entries table) :count 1)))
    (not (null entry))))

(defmethod clrtable ((table table))
  (setf (table-entries table) '()))

(defun convert-alist-to-table (alist &key (test #'eql))
  (make-table :entries (copy-tree alist)
	      :test test))
