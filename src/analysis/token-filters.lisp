(in-package #:montezuma)

(defclass token-filter (token-stream)
  ((input :initarg :input)))

(defmethod close ((self token-filter))
  (with-slots (input) self
    (close input)))

(defclass lowercase-filter (token-filter)
  ())

(defmethod next-token ((self lowercase-filter))
  (with-slots (input) self
    (let ((token (next-token input)))
      (when token
	(setf (token-image token) (nstring-downcase (token-image token))))
      token)))

(defclass stop-filter (token-filter)
  ((stop-set :initarg :stop-set :initform '())))

(defmethod initialize-instance :after ((self stop-filter) &key file)
  (when file
    (with-slots (stop-set) self
      (when stop-set
	(error ":stop-set and :file cannot both be specified for a stop-filter."))
      (setf stop-set (load-word-list file)))))

(defun load-word-list (path)
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil)
	 while line collect line)))


(defmethod next-token ((self stop-filter))
  (with-slots (input stop-set) self
    (let ((token (next-token input)))
      (if (or (null token)
	      (not (member (token-image token) stop-set :test #'string=)))
	  token
	  (next-token self)))))


(defclass porter-stem-filter (token-filter)
  ())

(defmethod next-token ((self porter-stem-filter))
  (with-slots (input) self
    (let ((token (next-token input)))
      (when token
	(setf (token-image token) (stem (token-image token))))
      token)))


	