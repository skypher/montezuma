(in-package montezuma)

(defparameter +default-max-clause-count+ 1024)

(define-condition too-many-clauses-error (error)
                  ())

(defclass boolean-query (query)
  ((max-clause-count :accessor )
   (clauses :accessor )
   (max-clause-count :initform +default-max-clause-count+)))

