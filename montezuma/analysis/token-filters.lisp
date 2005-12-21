(in-package #:montezuma)

(defclass token-filter (token-stream)
  ((input :initarg :input)))

(defmethod close-stream ((self token-filter))
  (with-slots (input) self
    (close-stream input)))

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


	