(in-package montezuma)

#+Ignore
;; defined in store's api
(defgeneric seek (term-doc-enum pos))

(defgeneric doc (term-doc-enum)
  (:documentation "Returns the current document number matching the query. Initially invalid, until #next?() is called the first time.
"))

(defgeneric freq (term-doc-enum))

(defgeneric next (term-doc-enum))

(defgeneric read-segment-term-doc-enum (term-doc-enum docs freqs &optional start))

(defgeneric term (enum))

(defgeneric doc-freq (enum))

(defgeneric (setf term-text) (text term))
(defgeneric (setf term-field) (field term))


(defgeneric do-commit (reader))
(defgeneric do-close (reader))
(defgeneric do-delete (reader doc-number))
(defgeneric do-undelete-all (reader))

(defgeneric deleted-p (reader doc-num))

(defgeneric get-field-names (reader &optional field-option))
(defgeneric fake-norms (reader))
(defgeneric get-norms-into (reader field buffer offset))
(defgeneric optimize (index-writer))

(defgeneric add-indexes (index-writer &rest dirs))
(defgeneric add-indexes-readers (index-writer readers))

(defgeneric skip-to (enum target))
(defgeneric next? (enum))
(defgeneric next-position (enum))

(defgeneric terms-from (source term))
(defgeneric terms (term-infos-reader))

(defgeneric get-document (index doc-number))

(defgeneric has-deletions-p (index-reader))
