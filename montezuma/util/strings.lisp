(in-package #:montezuma)

(defun bytes-to-string (sequence &key (start 0) (end (length sequence)))
  "Converts a sequence of bytes (unsigned-byte 8) to a string using
   the implementation's default character encoding."
  (let ((s (make-string (- end start))))
    (dotimes (i (- end start))
      (setf (char s i) (code-char (elt sequence (+ i start)))))
    s))


(defun string-to-bytes (string)
  "Converts a string to a sequence of bytes (unsigned-byte 8) using
   the implementation's default character encoding."
  (let ((s (make-array (list (length string)))))
    (dotimes (i (length string))
      (setf (elt s i) (char-code (char string i))))
    s))

(defun string-compare (s1 s2)
  (cond ((string< s1 s2) -1)
	((string> s1 s2) 1)
	(T 0)))
