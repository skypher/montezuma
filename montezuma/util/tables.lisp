(in-package #:montezuma)

(defstruct table
  entries
  test)

(defun table-entry (key table)
  (assoc key (table-entries table) :test (table-test table)))

(defun table-value (key table &optional value)
  (let ((entry (table-entry key table)))
    (if entry
	(cdr entry)
	value)))

(defun set-table-value (key table value)
  (let ((entry (table-entry key table)))
    (if entry
	(setf (cdr entry) value)
	(setf (table-entries table)
	      (append (table-entries table)
		      (list (cons key value)))))
    value))

(defsetf table-value set-table-value)

(defun in-table-p (key table)
  (not (null (table-entry key table))))

(defun table-values (table)
  (mapcar #'cdr (table-entries table)))

(defun table-keys (table)
  (mapcar #'car (table-entries table)))

(defun remtable (key table)
  (let ((entry (table-entry key table)))
    (when entry
      (setf (table-entries table) (cl:delete entry (table-entries table) :count 1)))
    (not (null entry))))

(defun clrtable (table)
  (setf (table-entries table) '()))
