(in-package #:montezuma)


(defun make-bit-vector ()
  (list 0))

(defun bit-set-p (bit-vector index)
  (logbitp index (car bit-vector)))

(defun set-bit (bit-vector index)
  (setf (ldb (byte 1 index) (car bit-vector)) 1))

(defun clear-bit (bit-vector index)
  (setf (ldb (byte 1 index) (car bit-vector)) 0))

(defun bit-vector-count (bit-vector)
  (logcount (car bit-vector)))

(defun write-bit-vector (bit-vector directory filename)
  (let ((output (create-output directory filename)))
    (unwind-protect
	 (write-string output (format nil "~S" (car bit-vector)))
      (close output))))

(defun read-bit-vector (directory filename)
  (let ((input (open-input directory filename)))
    (unwind-protect
	 (let ((bit-string (read-string input)))
	   (list (parse-integer bit-string)))
      (close input))))