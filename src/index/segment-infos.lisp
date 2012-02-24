(in-package #:montezuma)

(defclass segment-info ()
  ((name :initarg :name :accessor segment-info-name)
   (doc-count :initarg :doc-count :accessor doc-count)
   (directory :initarg :directory :accessor directory)))

(defmethod print-object ((self segment-info) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "~S" (segment-info-name self))))

(defgeneric segment-info= (segment-info other))

(defmethod segment-info= ((self segment-info) other)
  (with-slots (name doc-count) self
    (and (typep other 'segment-info)
	 (string= name (slot-value other 'name))
	 (eql doc-count (slot-value other 'doc-count)))))

(defmethod has-deletions-p ((si segment-info))
  (file-exists-p (directory si) (add-file-extension (segment-info-name si) "del")))

(defgeneric uses-compound-file-p (segment-info))

(defmethod uses-compound-file-p ((si segment-info))
  (file-exists-p (directory si) (add-file-extension (segment-info-name si) "cfs")))

(defgeneric has-separate-norms-p (segment-info))

(defmethod has-separate-norms-p ((si segment-info))
  (let ((name-pattern (format nil "~A.s" (segment-info-name si))))
    (some #'(lambda (file) (string-begins file name-pattern))
	  (files (directory si)))))


(defparameter *segment-format* -1)
(defparameter *segment-filename* "segments")
(defparameter *temporary-segment-filename* "segments.new")


(defclass segment-infos ()
  ((format)
   (version :initform (get-universal-time) :reader version)
   (counter :initform 0 :accessor counter)
   (elements :initform (make-array 0 :adjustable T :fill-pointer T))))

(defmethod print-object ((self segment-infos) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (let ((elements (slot-value self 'elements)))
    (format stream "~S segment-infos: ~S" (length elements) elements))))

(defgeneric clear (segment-infos))

(defmethod clear ((self segment-infos))
  (setf (slot-value self 'elements) (make-array 0 :adjustable T :fill-pointer T)))

(defmethod size ((self segment-infos))
  (with-slots (elements) self
    (length elements)))

(defgeneric delete-at (segment-infos index))

(defmethod delete-at ((self segment-infos) index)
  (with-slots (elements) self
    ;; FIXME truly atrocious
    (let ((new-elements (concatenate 'vector (subseq elements 0 index) (subseq elements (+ index 1)))))
    (setf elements (make-array (length new-elements) :adjustable T :fill-pointer T :initial-contents new-elements)))))

(defgeneric add-segment-info (segment-infos si))

(defmethod add-segment-info ((self segment-infos) si)
  (with-slots (elements) self
    (vector-push-extend si elements)))

(defgeneric segment-info (segment-infos index))

(defmethod segment-info ((self segment-infos) index)
  (with-slots (elements) self
    (elt elements index)))

(defgeneric (setf segment-info) (value segment-infos index))

(defmethod (setf segment-info) (value (self segment-infos) index)
  (with-slots (elements) self
    (assert (< index (length elements)))
    (setf (elt elements index) value)))

(defmethod initialize-copy :after ((self segment-infos) other)
  (dotimes (i (size other))
    (setf (segment-info self i) (clone (segment-info other i)))))

(defun segment-infos-read-current-version (directory)
  (if (not (file-exists-p directory *segment-filename*))
      0
      (let ((format nil)
	    (version nil))
	(let ((input (open-input directory *segment-filename*)))
	  (unwind-protect
	       (progn
		 (setf format 0)
		 (setf version 0)
		 (setf format (read-int input))
		 (when (< format 0)
		   (when (< format *segment-format*)
		     (error "Unknown format version ~S" format))
		   (setf version (read-long input))))
	    (close input)))
	(if (< format 0)
	    version
	    (let ((sis (make-instance 'segment-infos)))
	      (read-segment-infos sis directory)
	      (version sis))))))
	
(defgeneric read-segment-infos (segment-infos directory))

(defmethod read-segment-infos ((self segment-infos) directory)
  (let ((input (open-input directory *segment-filename*)))
    (unwind-protect
	 (with-slots (format counter version elements) self
	   (setf format (read-int input))
	   (if (< format 0)
	       (progn
		 (when (< format *segment-format*)
		   (error "Unknown format version ~S" format))
		 (setf version (read-long input))
		 (setf counter (read-int input)))
	       (setf counter format))
	   (let ((seg-count (read-int input)))
	     (dotimes (i seg-count)
	       (add-segment-info self
				 (make-instance 'segment-info
						:name (read-string input)
						:doc-count (read-int input)
						:directory directory))))
	   (when (>= format 0)
	     (if (>= (pos input) (size input))
		 (setf version 0)
		 (setf version (read-long input)))))
      (close input))))

(defgeneric write-segment-infos (segment-infos directory))


(defmethod write-segment-infos ((self segment-infos) directory)
  (let ((output (create-output directory *temporary-segment-filename*)))
    (unwind-protect
	 (with-slots (version counter elements) self
	   (write-int output *segment-format*)
	   (write-long output (incf version))
	   (write-int output counter)
	   (write-int output (size self))
	   (dotimes (i (length elements))
	     (let ((si (segment-info self i)))
	       (write-string output (segment-info-name si))
	       (write-int output (doc-count si)))))
      (close output)))
  (rename-file directory *temporary-segment-filename* *segment-filename*))
