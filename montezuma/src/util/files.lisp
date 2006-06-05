(in-package #:montezuma)

(defun add-file-extension (file extension)
  (concatenate 'string file "." extension))
