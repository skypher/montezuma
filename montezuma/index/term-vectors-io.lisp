(in-package #:montezuma)

(defparameter *term-vectors-format-version* 2)
(defparameter *term-vectors-format-size* 4)

(defparameter *tvx-extension* (make-pathname :type "tvx"))
(defparameter *tvd-extension* (make-pathname :type ".tvd"))
(defparameter *tvf-extension* (make-pathname :type ".tvf"))

(defparameter +store-positions-with-term-vector+ #x1)
(defparameter +store-offset-with-term-vector+ #x2)

(defstruct tv-field
  (tvf-pointer 0)
  number
  store-positions
  store-offsets)



(defclass term-vectors-writer ()
  (
   (current-field :initform nil)
   (current-doc-pointer :initform -1)
   (tvx)
   (tvd)
   (tvf)
   (field-infos :initarg :field-infos)
   (fields :initform (make-array 0 :adjustable T :fill-pointer T))
   (terms :initform (make-array 0 :adjustable T :fill-pointer T))
   ))


(defmethod initialize-instance :after ((self term-vectors-writer) &key directory segment)
  (with-slots (tvx tvd tvf) self
    (setf tvx (create-output directory (merge-pathnames *tvx-extension* segment)))
    (write-int tvx *term-vectors-format-version*)
    (setf tvd (create-output directory (merge-pathnames *tvd-extension* segment)))
    (write-int tvd *term-vectors-format-version*)
    (setf tvf (create-output directory (merge-pathnames *tvf-extension* segment)))
    (write-int tvf *term-vectors-format-version*)))

(defmethod open-document ((self term-vectors-writer))
  (with-slots (current-doc-pointer tvd) self
    (close-document self)
    (setf current-doc-pointer (pos tvd))))

(defmethod close-document ((self term-vectors-writer))
  (with-slots (fields current-doc-pointer) self
    (when (document-open-p self)
      (close-field self)
      (write-doc self)
      (setf fields (make-array 0 :adjustable T :fill-pointer T))
      (setf current-doc-pointer -1))))

(defmethod document-open-p ((self term-vectors-writer))
  (with-slots (current-doc-pointer) self
    (not (= current-doc-pointer -1))))


(defmethod open-field ((self term-vectors-writer) field)
  (with-slots (field-infos) self
    (let ((field-info (get-field field-infos field)))
      (create-field self
		    (field-number field-info)
		    (field-store-positions-p field-info)
		    (field-store-offsets-p field-info)))))

(defmethod close-field ((self term-vectors-writer))
  (when (field-open-p self)
    (with-slots (fields terms current-field) self
      (write-field self)
      (vector-push-extend current-field fields)
      (setf terms (make-array 0 :adjustable T :fill-pointer T))
      (setf current-field nil))))

(defmethod field-open-p ((self term-vectors-writer))
  (with-slots (current-field) self
    (not (null current-field))))

(defmethod add-term-to-term-vectors-writer ((self term-vectors-writer) term-text freq &key positions offsets)
  (unless (document-open-p self)
    (error "Cannot add terms when document is not open."))
  (unless (field-open-p self)
    (error "Cannot add terms when field is not open."))
  (add-term-internal self term-text freq positions offsets))

(defmethod add-term-internal ((self term-vectors-writer) term-text freq positions offsets)
  (with-slots (terms) self
    (vector-push-extend (make-instance 'tv-term
				       :term-text term-text
				       :freq freq
				       :positions positions
				       :offsets offsets)
			terms)))

(defmethod add-all-doc-vectors ((self term-vectors-writer) vectors)
  (with-slots (field-infos) self
    (open-document self)
    (when vectors
      (dolist (vector vectors)
	(let ((store-positions (and (> (size vector) 0) (positions vector)))
	      (store-offsets (and (> (size vector) 0) (offsets vector))))
	  (create-field self 
			(get-field-number field-infos (field vector))
			store-positions
			store-offsets)
	  (dotimes (j (size vector))
	    (add-term-internal self
			       (aref (terms vector) j)
			       (if store-positions (aref (positions vector) j) nil)
			       (if store-offsets (aref (offsets vector) j) nil)))
	  (close-field self))))
    (close-document self)))

(defmethod close ((self term-vectors-writer))
  ;; FIXME: handle errors better?
  (close-document self)
  (with-slots (tvx tvd tvf) self
    (close tvx)
    (close tvd)
    (close tvf)))


(defmethod write-field ((self term-vectors-writer))
  (with-slots (current-field tvf terms) self
    (setf (tv-field-tvf-pointer current-field) (pos tvf))
    (write-vint tvf (length terms))
    (let ((store-positions (tv-field-store-positions current-field))
	  (store-offsets (tv-field-store-offsets current-field))
	  (bits #x0))
      (when store-positions
	(setf bits (logior bits +store-positions-with-term-vector+)))
      (when store-offsets
	(setf bits (logior bits +store-offset-with-term-vector+)))
      (write-byte tvf bits)
      (let ((last-term-text ""))
	(dotimes (i (length terms))
	  (let* ((term (aref terms i))
		 (start (or (mismatch last-term-text (term-text term))
			    (min (length last-term-text) (length (term-text term)))))
		 (length (- (length (term-text term)) start)))
	    (write-vint tvf start)
	    (write-vint tvf length)
	    (write-chars tvf (string-to-bytes (term-text term)) start length)
	    (write-vint tvf (freq term))
	    (setf last-term-text (term-text term))

	    (when store-positions
	      (unless (positions term)
		(error "Can't write null positions"))
	      (let ((position 0))
		(dotimes (j (freq term))
		  (write-vint tvf (- (aref (positions term) j) position))
		  (setf position (aref (positions term) j)))))
	    (when store-offsets
	      (unless (offsets term)
		(error "Can't write null offsets."))
	      (let ((position 0))
		(dotimes (j (freq term))
		  (write-vint tvf (- (start-offset (aref (offsets term) j)) position))
		  (write-vint tvf (- (end-offset (aref (offsets term) j))
				     (start-offset (aref (offsets term) j))))
		  (setf position (end-offset (aref (offsets term) j))))))))))))

(defmethod write-doc ((self term-vectors-writer))
  (when (field-open-p self)
    (error "Field is still open while writing document"))
  (with-slots (tvx current-doc-pointer fields tvd) self
    (write-long tvx current-doc-pointer)
    (let ((size (length fields)))
      (write-vint tvd size)
      (dotimes (i (length fields))
	(let ((field (aref fields i)))
	  (write-vint tvd (tv-field-number field))))
      (let ((last-field-pointer 0))
	(dotimes (i (length fields))
	  (let ((field (aref fields i)))
	    (write-vlong tvd (- (tv-field-tvf-pointer field) last-field-pointer))
	    (setf last-field-pointer (tv-field-tvf-pointer field))))))))

(defmethod create-field ((self term-vectors-writer) field-number store-position store-offset)
  (unless (document-open-p self)
    (error "Cannot open field when no document is open."))
  (close-field self)
  (with-slots (current-field) self
    (setf current-field (make-tv-field :number field-number
				       :store-positions store-position
				       :store-offsets store-offset))))

	    


(defclass tv-term ()
  ((term-text :initform nil :initarg :term-text :reader term-text)
   (freq :initform nil :initarg :freq :reader freq)
   (positions :initform nil :initarg :positions :reader positions)
   (offsets :initform nil :initarg :offsets :reader offsets)))


(defclass term-vectors-reader ()
  ((field-infos :initarg :field-infos)
   (tvx)
   (tvd)
   (tvd-format)
   (tvf)
   (tvf-format)
   (size)))

(defmethod initialize-instance :after ((self term-vectors-reader) &key dir segment)
  (with-slots (tvx tvd tvd-format tvf tvf-format size) self
    (if (file-exists-p dir (merge-pathnames *tvx-extension* segment))
	(progn
	  (setf tvx (open-input dir (merge-pathnames *tvx-extension* segment)))
	  (check-valid-format self tvx)
	  (setf tvd (open-input dir (merge-pathnames *tvd-extension* segment)))
	  (setf tvd-format (check-valid-format self tvd))
	  (setf tvf (open-input dir (merge-pathnames *tvf-extension* segment)))
	  (setf tvf-format (check-valid-format self tvf))
	  (setf size (/ (size tvf) 8)))
	(setf tvx nil
	      tvd nil
	      tvf nil))))

(defmethod close ((self term-vectors-reader))
  ;; FIXME handle errors?
  (with-slots (tvx tvd tvf) self
    (close tvx)
    (close tvd)
    (close tvf)))

(defmethod get-field-tv ((self term-vectors-reader) doc-num field)
  (with-slots (field-infos tvx tvd-format tvd) self
    (let ((field-number (get-field-number field-infos field))
	  (result nil))
      (when tvx
	(seek tvx (+ (* doc-num 8) *term-vectors-format-size*))
	(let ((position (read-long tvx)))
	  (seek tvd position)
	  (let ((field-count (read-vint tvd))
		(number 0)
		(found -1))
	    (dotimes (i field-count)
	      (if (= tvd-format *term-vectors-format-version*)
		  (setf number (read-vint tvd))
		  (incf number (read-vint tvd)))
	      (when (= number field-number)
		(setf found i)))
	    (when (not (= found -1))
	      (setf position 0)
	      (dotimes (i (+ found 1))
		(incf position (read-vlong tvd)))
	      (setf result (read-term-vector self field position))))))
      result)))

(defmethod get-tv ((self term-vectors-reader) doc-num)
  (let ((result nil))
    (with-slots (tvx tvd tvd-format field-infos) self
      (when tvx
	(seek tvx (+ (* doc-num 8) *term-vectors-format-size*))
	(let ((position (read-long tvx)))
	  (seek tvd position)
	  (let ((field-count (read-vint tvd)))
	    (when (not (= field-count 0))
	      (let ((number 0)
		    (fields (make-array field-count)))
		(dotimes (i field-count)
		  (if (= tvd-format *term-vectors-format-version*)
		      (setf number (read-vint tvd))
		      (incf number (read-vint tvd)))
		  (setf (aref fields i) (name (aref field-infos number))))
		(setf position 0)
		(let ((tvf-pointers (make-array field-count)))
		  (dotimes (i field-count)
		    (incf position (read-vlong tvd))
		    (setf (aref tvf-pointers i) position))
		  (setf result (read-term-vectors self fields tvf-pointers)))))))))
    result))

(defmethod clone-object ((self term-vectors-reader))
  (with-slots (tvx tvd tvf) self
    (if (or (null tvx) (null tvd) (null tvf))
	nil
	(let ((clone self))
	  (setf tvx (clone tvx))
	  (setf tvd (clone tvd))
	  (setf tvf (clone tvf))
	  clone))))

(defmethod read-term-vectors ((self term-vectors-reader) fields tvf-pointers)
  (let ((tvs (make-array (length fields))))
    (dotimes (i (length fields))
      (setf (aref tvs i) (read-term-vector self (aref fields i) (aref tvf-pointers i))))
    tvs))


(defmethod read-term-vector ((self term-vectors-reader) field tvf-pointer)
  (with-slots (tvf) self
    (seek tvf tvf-pointer)
    (let ((num-terms (read-vint tvf)))
      (if (= num-terms 0)
	  (make-instance 'segment-term-vector (error "FOO") field nil nil)
	  (let ((store-positions NIL)
		(store-offsets NIL))
	    (if (= tvf-format *term-vectors-format-version*)
		(let ((bits (read-byte tvf)))
		  (setf store-positions (logbitp +store-positions-with-term-vector+ bits))
		  (setf store-offsets (logbitp +store-offset-with-term-vector+ bits)))
		(read-vint tvf))
	    (let ((terms (make-array num-terms))
		  (term-freqs (make-array num-terms)))
	      (let ((positions (if store-positions (make-array num-terms) nil))
		    (offsets (if store-offsets (make-array num-terms) nil)))
		(let ((start 0)
		      (delta-length 0)
		      (total-length 0)
		      (buffer "")
		      (previous-buffer ""))
		  (dotimes (i num-terms)
		    (let* ((start (read-vint tvf))
			   (delta-length (read-int tvf))
			   (total-length (+ start delta-length)))
		      (read-chars tvf buffer start delta-length)
		      (setf (aref terms i) (subseq buffer 0 total-length))
		      (setf previous-buffer (aref terms i))
		      (setf freq (read-vint tvf))
		      (setf (aref term-freqs i) freq)

		      (when store-positions
			(let ((pos (make-array freq)))
			  (setf (aref positions i) pos)
			  (setf prev-position 0)
			  (dotimes (j freq)
			    (setf (aref pos j) (+ prev-position (read-vint tvf)))
			    (setf prev-position (aref pos j)))))
		      
		      (when store-offsets
			(let ((offs (make-array freq)))
			  (setf (aref offsets i) offs)
			  (setf prev-offset 0)
			  (dotimes (j freq)
			    (let* ((start-offset (+ prev-offset (read-vint tvf)))
				   (end-offset (+ start-offset (read-vint tvf))))
			      (setf (aref offs j) (make-instance 'term-vector-offset-info
								 :start-offset start-offset
								 :end-offset end-offset))
			      (setf prev-offset end-offset))))))))
		(make-instance 'segment-term-vector (error "FOO") field terms term-freqs positions offsets))))))))

(defmethod check-valid-format ((self term-vectors-reader) input-stream)
  (let ((format (read-int input-stream)))
    (if (> format *term-vectors-format-version*)
	(error "Incompatible format version ~S, expected ~S" format *term-vectors-format-version*)
	format)))
