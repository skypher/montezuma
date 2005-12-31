(in-package #:montezuma)

(defun stream-contents (stream)
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
	   (buffer (make-string buffer-size)))
      (loop for size = (read-sequence buffer stream)
	 do (cl:write-string buffer contents :start 0 :end size)
	 while (= size buffer-size)))))
