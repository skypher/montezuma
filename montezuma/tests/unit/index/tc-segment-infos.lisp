(in-package #:montezuma)

(deftestfun test-segment-infos
  (let ((dir (make-instance 'ram-directory)))
    (let ((sis (make-instance 'segment-infos)))
      (test segment-infos-1 (read-current-version-segment-infos sis dir) 0)
      (let ((seg0 (make-instance 'segment-info
				 :name "seg0"
				 :doc-count 5
				 :directory dir))
	    (seg1 (make-instance 'segment-info
				 :name "seg1"
				 :doc-count 5
				 :directory dir))
	    (seg2 (make-instance 'segment-info
				 :name "seg2"
				 :doc-count 5
				 :directory dir))
	    (seg3 (make-instance 'segment-info
				 :name "seg3"
				 :doc-count 5
				 :directory dir)))
	(add-segment-info sis seg0)
	(add-segment-info sis seg1)
	(add-segment-info sis seg2)
	(test segment-infos-2 (size sis) 3)
	(test segment-infos-3 (segment-info sis 0) seg0 #'segment-info=)
	(test segment-infos-4 (segment-info sis 2) seg2 #'segment-info=)
	(write-segment-infos sis dir)
	(let ((version (read-current-version-segment-infos sis dir)))
	  (test segment-infos-5 (and (file-exists-p dir "segments") T) T)
	  (let ((sis2 (make-instance 'segment-infos)))
	    (read-segment-infos sis2 dir)
	    (test segment-infos-6 (size sis2) 3)
	    (test segment-infos-7 (segment-info sis 0) seg0 #'segment-info=)
	    (test segment-infos-8 (segment-info sis 2) seg2 #'segment-info=)
	    (add-segment-info sis2 seg3)
	    (write-segment-infos sis2 dir)
	    (test segment-infos-9 (read-current-version-segment-infos sis2 dir) (+ version 1)))
	  (let ((sis3 (make-instance 'segment-infos)))
	    (read-segment-infos sis3 dir)
	    (test segment-infos-10 (size sis3) 4)
	    (test segment-infos-11 (segment-info sis3 0) seg0 #'segment-info=)
	    (test segment-infos-12 (segment-info sis3 3) seg3 #'segment-info=)))))))

(defparameter *test-directory-path*
  (make-pathname :name nil
		 :type nil
		 :defaults (merge-pathnames (make-pathname :directory '(:relative :up :up "temp" "fsdir"))
					    *load-pathname*)))

(deftestfun test-segment-info
  (let ((dir (make-instance 'ram-directory)))
    (let ((si (make-instance 'segment-info
			     :name "seg1"
			     :doc-count 0
			     :directory dir)))
      (test segment-info-1 (directory si) dir)
      (test segment-info-2 (doc-count si) 0)
      (test segment-info-3 (segment-info-name si) "seg1" #'string=)
      (close dir)
      (let ((dir (make-fs-directory *test-directory-path* :create-p T)))
	(setf (segment-info-name si) "seg2")
	(incf (doc-count si) 2)
	(setf (directory si) dir)
	(test segment-info-4 (directory si) dir)
	(test segment-info-5 (doc-count si) 2)
	(test segment-info-6 (segment-info-name si) "seg2" #'string=)))))

