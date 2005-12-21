(in-package #:montezuma)

(defclass token-stream ()
  ())

(defgeneric next-token (token-stream))

(defgeneric close-stream (token-stream))
