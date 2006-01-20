(in-package #:montezuma)

(defun add-file-extension (file extension)
  (unless (pathnamep extension)
    (setf extension (make-pathname :type extension)))
  (merge-pathnames extension file))

		   