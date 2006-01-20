(in-package #:montezuma)

(defparameter *test-directory-path*
  (make-pathname :name nil
		 :type nil
		 :defaults (merge-pathnames (make-pathname :directory '(:relative :up :up "temp" "fsdir"))
					    *load-pathname*)))

(print *test-directory-path*)

(deftestfun test-fs-store
  (let ((dir (make-fs-directory *test-directory-path* :create-p T)))
    (do-test-basic-file-ops dir)
    (do-test-rename dir)
    (do-test-modified dir)
    (do-test-rw-bytes dir)
    (do-test-rw-ints dir)
    (do-test-rw-longs dir)
    (do-test-rw-uints dir)
    (do-test-rw-ulongs dir)
    (do-test-rw-vints dir)
    (do-test-rw-vlongs dir)
    (do-test-rw-strings dir)
    (do-test-buffer-seek dir)
    (do-test-read-bytes dir)
    (do-test-clone dir)))

