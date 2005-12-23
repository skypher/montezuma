(in-package #:montezuma)

(defclass directory ()
  ((lock-prefix :initarg :lock-prefix))
  (:default-initargs
   :lock-prefix "montezuma-"))

(defgeneric files (directory))

(defmethod file-count ((self directory))
  (length (files self)))

(defgeneric file-exists-p (directory file))

(defgeneric modified-time (directory file))

(defgeneric touch (directory file))

(defgeneric delete-file (directory file))

(defgeneric rename-file (directory from to))

(defgeneric file-size (directory file))

(defgeneric create-output (directory filename))

(defgeneric open-input (directory filename))

(defgeneric make-lock (directory lock-name))

(defgeneric close (directory))


(defclass lock ()
  ((max-attempts :initarg :max-attempts))
  (:default-initargs
   :max-attempts 5))

(defgeneric obtain (lock &optional timeout))

(defgeneric release (lock))

(defgeneric locked-p (lock))

(defmacro with-lock ((lock) &body body)
  (let ((lock-var (gensym "LOCK")))
    `(let ((,lock-var ,lock))
       (obtain ,lock-var)
       (unwind-protect
	    (progn ,@body)
	 (release ,lock-var)))))

(defmacro do-files ((file-var directory) &body body)
  `(dolist (,file-var (files ,directory))
     ,@body))
