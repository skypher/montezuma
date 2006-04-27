(in-package montezuma-lift)

(defun make-keyword-field (name data)
  (make-instance 'field 
    :name name :data data :stored-p t :indexed-p t))

(defun make-unindexed-field (name data)
  (make-instance 'field 
    :name name :data data :stored-p t :indexed-p nil))

(defun make-unstored-field (name data)
  (make-instance 'field 
    :name name :data data :stored-p t :indexed-p nil))

(defun make-text-field (name data)
  (make-instance 'field 
    :name name :data data :stored-p nil :indexed-p t))