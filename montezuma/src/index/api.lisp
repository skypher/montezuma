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
