(in-package montezuma)

(defclass index-searcher ()
  ((similarity :accessor similarity :initarg :similarity)
   (reader :accessor reader)
   (directory :accessor directory))
  (:default-initargs
    :similarity (make-default-similarity)))

(defmethod initialize-instance :after ((self index-searcher) &key reader)
  (setf (slot-value self 'reader) (initialize-reader self reader)))

(defmethod initialize-reader ((self index-searcher) (reader index-reader))
  (values reader))

(defmethod initialize-reader ((self index-searcher) (reader directory))
  (open-index-reader reader :close-directory-p false))

(defmethod initialize-reader ((self index-searcher) (reader string))
  (setf (directory self) (make-fs-directory reader))
  (open-index-reader (directory self) :close-directory-p t))

(defmethod close ((self index-searcher))
  ;; delegate
  (close (reader self)))

(defmethod term-doc-freq ((self index-searcher) (term term))
  ;; delegate
  (term-doc-freq (reader self) term))

(defmethod term-doc-freqs ((self index-searcher) (terms sequence))
  (let ((result (make-array (length terms))))
    (dosequence (i terms)
      (setf (aref i result) 
            (term-doc-freq self (aref terms i))))
    (values result)))

(defmethod get-document ((self index-searcher) index)
  ;; delegate
  (get-document (reader self) index))

(defmethod max-doc ((self index-searcher))
  ;; delegate
  (max-doc (reader self)))

(defmethod create-weight ((self index-searcher) (query query))
  (weight query self))

(defmethod search ((self index-searcher) (query query) 
                   &optional options)
  (destructuring-bind (&key (filter nil) (first-document 0) (num-documents 10)
                            (max-size (+ first-document num-documents))
                            (sort nil)) options
    
    (assert (plusp num-documents))
    (assert (not (minusp first-document)))
    (let ((scorer (scorer (weight query self) (reader self))))
      (when (null scorer)
        (return-from search (make-instance 'top-docs)))
      )))

(defmethod search-each ((self index-searcher) (query query) &optional (options nil))
  )

(defmethod rewrite ((self index-searcher) original)
  (let* ((query original)
         (rewritten-query (rewrite query (reader self))))
    (while (not (equal query rewritten-query))
      (setf query rewritten-query
            rewritten-query (rewrite query (reader self))))
    (values query)))

(defmethod explain ((self index-searcher) (query query) index)
  ;; not implemented
  (error "not yet implemented"))