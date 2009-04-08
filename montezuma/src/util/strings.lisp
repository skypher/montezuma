(in-package #:montezuma)

(defun bytes-to-string (sequence &key (start 0) (end (length sequence)))
  "Converts a sequence of octets to a string using the implementation's
default character encoding."
   (let ((s (coerce (subseq sequence start end) '(vector (unsigned-byte 8)))))
     (babel:octets-to-string s)))


(defun string-to-bytes (string &key (start 0) end)
  "Converts a string to a sequence of octets using the implementation's
default character encoding.

NB: START and END denote bounds for the final octet vector,
not for STRING."
  (let ((s (babel:string-to-octets string)))
    (subseq s start (or end (length s)))))


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
