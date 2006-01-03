(in-package #:montezuma)

(defparameter *test-directory-path*
  (make-pathname :name nil
		 :type nil
		 :defaults (merge-pathnames (make-pathname :directory '(:relative :up :up "temp" "fsdir"))
					    *load-pathname*)))

(print *test-directory-path*)

(deftestfun test-fs-store
  (let ((dir (make-fs-directory *test-directory-path* :create-p T)))
    (test-basic-file-ops dir)
    (test-rename dir)
    (test-modified dir)
    (test-rw-bytes dir)
    (test-rw-ints dir)
    (test-rw-longs dir)
    (test-rw-uints dir)
    (test-rw-ulongs dir)
    (test-rw-vints dir)
    (test-rw-vlongs dir)
    (test-rw-strings dir)
    (test-buffer-seek dir)
    (test-read-bytes dir)
    (test-clone dir)))

