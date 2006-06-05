(in-package #:montezuma)

(defun add-file-extension (file extension)
  (assert (stringp file))
  (assert (stringp extension))
  (unless (pathnamep extension)
    (setf extension (make-pathname :type extension)))
  (namestring (merge-pathnames extension file)))
