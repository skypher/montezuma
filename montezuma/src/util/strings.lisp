(in-package #:montezuma)

(defun bytes-to-string (sequence &key (start 0) (end (length sequence)))
  "Converts a sequence of bytes (unsigned-byte 8) to a string using
   the implementation's default character encoding."
  (let ((s (make-string (- end start))))
    (dotimes (i (- end start))
      (setf (char s i) (code-char (elt sequence (+ i start)))))
    s))


(defun string-to-bytes (string &key (start 0) (end (length string)))
  "Converts a string to a sequence of bytes (unsigned-byte 8) using
   the implementation's default character encoding."
  (let ((s (make-array (list (- end start)))))
    (dotimes (i (- end start))
      (setf (elt s i) (char-code (char string (+ i start)))))
    s))


(defun string-compare (s1 s2  &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (let ((pos (mismatch s1 s2 :test #'char= :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
    (if (null pos)
	0
	(cond ((>= pos end1)
	       -1)
	      ((>= pos end2)
	       1)
	      (T (if (char< (char s1 pos) (char s2 pos))
		     -1
		     1))))))


(defun make-adjustable-string (length &optional s)
  (if s
      (make-array (list length)
		  :element-type 'character
		  :fill-pointer T
		  :adjustable T
		  :initial-contents s)
      (make-array (list length)
		  :element-type 'character
		  :fill-pointer T
		  :adjustable T)))


(defun string-begins (string pattern)
  (let ((m (mismatch string pattern :test #'char=)))
    (or (null m) (= m (length pattern)))))
