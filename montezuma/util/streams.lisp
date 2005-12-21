(in-package #:montezuma)

(defun stream-contents (stream)
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
	   (buffer (make-string buffer-size)))
      (labels ((read-chunks ()
		 (let ((size (read-sequence buffer stream)))
		   (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
	(read-chunks)))))
