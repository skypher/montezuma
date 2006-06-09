;;;
;;; Copyright (c) 2003-2005, Gigamonkeys Consulting All rights reserved.
;;;
;;; Parser generator, loosely based on Henry Baker's META paper. The
;;; most obvious change is that we don't user reader macros because
;;; they're too much of a hassle.
;;;

(in-package #:montezuma.parser)

(defvar *productions* (make-hash-table :test #'eql))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsers -- different subclasses of PARSER differ based on the kind
;; of input they consume. CHARACTER-PARSERs consume characters and can
;; turn them into a vector of tokens or can proceed directly to a tree
;; or object representation. TOKEN-PARSERS consume tokens (such as
;; might be produced by a CHARACTER-PARSER but not necessarily) and
;; return whatever.

(defclass parser ()
  ((name    :initarg :name :reader name)
   (grammar :initarg :grammar :reader grammar)
   (tokens  :initform nil :initarg :tokens :accessor tokens)))

(defclass character-parser (parser) ()) ;; i.e. a lexer

(defclass token-parser (parser) ()) ;; i.e. a parser

(defclass token ()
  ((kind :initform nil :initarg :kind :accessor kind)
   (value :initform nil :initarg :value :accessor value)))

(defmethod print-object ((self token) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "~S ~S" (kind self) (value self))))

(defgeneric is-token (parser token-descriptor))

(defmethod is-token ((parser parser) (token-descriptor string))
  (member token-descriptor (tokens parser) :test #'string=))

(defmethod is-token ((parser parser) (token-descriptor symbol))
  (member token-descriptor (tokens parser) :test #'eql))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammars

(defclass grammar () ())

(defclass literal-value-grammar (grammar)
  ((value :initarg :value :reader value)))

(defclass character-grammar (literal-value-grammar) ())

(defclass string-grammar (literal-value-grammar) ())

(defclass production-call-grammar (grammar)
  ((name :initarg :name :reader name)))

(defclass sequence-grammar (grammar)
  ((sub-grammars :initarg :sub-grammars :reader sub-grammars)))

(defclass alternative-grammar (sequence-grammar) ())
(defclass conjunctive-grammar (sequence-grammar) ())

(defclass composite-grammar (grammar)
  ((sub-grammar :initarg :sub-grammar :reader sub-grammar)))

(defclass optional-grammar    (composite-grammar) ())
(defclass star-grammar        (composite-grammar) ())
(defclass plus-grammar        (composite-grammar) ())
(defclass negative-grammar    (composite-grammar) ())
(defclass not-match-grammar   (negative-grammar) ())

(defclass build-grammar (grammar)
  ((grammar :initarg :grammar :reader grammar)
   (form    :initarg :form :reader form)))

(defclass token-match-grammar (grammar)
  ((name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productions -- two kinds, chartypes that use the Lisp type system,
;; and grammar productions that use our grammar language.

(defclass production ()
  ((name :initarg :name :reader name)))

(defclass chartype-production (production) ())

(defclass grammar-production (production)
  ((variables :initarg :variables :reader variables)
   (grammar   :initarg :grammar   :reader grammar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert an s-expression representation of a grammar into the
;;; grammar objects we actually use for generating code. (We could go
;;; in one step from s-expressions to code but there's just enough
;;; complexity in the code generation that it's nice to have objects
;;; that are a bit easier to deal with. This also allows the
;;; possibility of using a different surface syntax with the same
;;; intermediate representation.)

(defgeneric make-grammar (sexp)
  (:documentation "Make a tree of grammar objects from an s-expression."))

(defmethod make-grammar ((sexp character))
  (make-instance 'character-grammar :value sexp))

(defmethod make-grammar ((sexp string))
  (make-instance 'string-grammar :value sexp))

(defmethod make-grammar ((sexp symbol))
  (make-instance 'production-call-grammar :name sexp))

(defmethod make-grammar ((sexp cons))
  (make-special-grammar (car sexp) sexp))

(defgeneric make-special-grammar (kind sexp))

(defmethod make-special-grammar ((kind (eql '?)) sexp)
  (make-instance 'optional-grammar :sub-grammar (make-grammar (rest sexp))))

(defmethod make-special-grammar ((kind (eql '*)) sexp)
  (make-instance 'star-grammar :sub-grammar (make-grammar (rest sexp))))

(defmethod make-special-grammar ((kind (eql '+)) sexp)
  (make-instance 'plus-grammar :sub-grammar (make-grammar (rest sexp))))

(defmethod make-special-grammar ((kind (eql '~)) sexp)
  (make-instance 'negative-grammar :sub-grammar (make-grammar (rest sexp))))

(defmethod make-special-grammar ((kind (eql '!)) sexp)
  (make-instance 'not-match-grammar :sub-grammar (make-grammar (rest sexp))))

(defmethod make-special-grammar ((kind (eql '/)) sexp)
  (make-instance 'alternative-grammar :sub-grammars (mapcar #'make-grammar (rest sexp))))

(defmethod make-special-grammar ((kind (eql '&)) sexp)
  (make-instance 'conjunctive-grammar :sub-grammars (mapcar #'make-grammar (rest sexp))))
    
(defmethod make-special-grammar ((kind (eql '@)) sexp)
  (make-instance 'build-grammar :grammar (make-grammar (second sexp)) :form (third sexp)))

(defmethod make-special-grammar ((kind (eql '^)) sexp) ;; extract is just sugar around build
  (make-instance 'build-grammar
    :grammar (make-grammar (second sexp))
    :form `(setf result ,(or (third sexp) 'last-match))))

(defmethod make-special-grammar ((kind (eql '%)) sexp)
  (make-instance 'token-match-grammar :name (second sexp)))

(defmethod make-special-grammar ((kind t) sexp)
  (if (not (rest sexp))
      (make-grammar (first sexp))
      (make-instance 'sequence-grammar :sub-grammars (mapcar #'make-grammar sexp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand bits of grammar into code to do the matching. Each kind of
;; grammar object is expanded into an expression that tries to match
;; the appropriate input and then 

(defgeneric code (grammar parser &key save-p)
  (:documentation "Emit the code to match the gramar"))

(defmethod code ((grammar character-grammar) (parser character-parser) &key save-p)
  (with-slots (value) grammar
    `(and
      (< index end)
      (char= (char input index) ,value)
      ,@(if save-p `((progn (setf last-match (string ,value)) t)))
      (progn (incf index) t))))

(defmethod code ((grammar character-grammar) (parser token-parser) &key save-p)
  (with-slots (value) grammar
    `(and
      (< index end)
      (let ((token (aref input index)))
        (and
         (string= (value token) (string ,value))
         ,@(if save-p `((progn (setf last-match token) t)))))
      (progn (incf index) t))))

(defmethod code ((grammar string-grammar)
                 (parser character-parser)
                 &key save-p)
  (with-slots (value) grammar
    `(let ((start index))
      (or
       (and
        ,@(map 'list #'(lambda (c) (code (make-grammar c) parser :save-p nil)) value)
        ,@(if save-p `((progn (setf last-match ,value) t)))
        ,@(if (is-token parser value)
              `((progn (vector-push-extend
                        (make-instance 'token :kind 'string-literal :value ,value) 
                        output) t))))
       (progn (setf index start) nil)))))

(defmethod code ((grammar string-grammar) (parser token-parser) &key save-p)
  (with-slots (value) grammar
    `(and
      (< index end)
      (let ((token (aref input index)))
        (and
         (string= (value token) ,value)
         ,@(if save-p `((progn (setf last-match token) t)))))
      (progn (incf index) t))))


(defmethod code ((grammar production-call-grammar) (parser parser) &key save-p)
  (with-slots (name) grammar
    (if (and (or save-p (is-token parser name)) (not (eql save-p 'not)))
        `(and (,name :save-p t)
          ,@(when save-p `((progn (setf ,name last-match) t)))
          ,@(when (is-token parser name)
                  `((progn
                      (vector-push-extend
                       (make-instance 'token :kind ',name :value last-match)
                       output)
                      t))))
        `(,name))))

(defmethod code ((grammar sequence-grammar) (parser parser) &key save-p)
  `(let ((start index))
     (or
      (and
       ,@(loop for g in (sub-grammars grammar)
	    collect (code g parser :save-p save-p)))
      (progn (setf index start) nil))))

(defmethod code ((grammar alternative-grammar) (parser parser) &key save-p)
  `(or 
    ,@(loop for g in (sub-grammars grammar)
	 collect (code g parser :save-p save-p))))

(defmethod code ((grammar conjunctive-grammar) (parser parser) &key save-p)
  (destructuring-bind (first &rest rest) (sub-grammars grammar)
    `(let ((start index) (end-first-match nil))
      (or (and
           (and
            ,(code first parser :save-p save-p)
            (progn (setf end-first-match index) t))
           ,@(mapcar
              #'(lambda (g) 
                  `(progn
                    (let ((end-match index))
                      (declare (ignorable end-match))
                      (setf index start)
                      ,(code g parser :save-p save-p))))
              rest)
           (progn (setf index end-first-match) t))
       (progn (setf index start) nil)))))

(defmethod code ((grammar optional-grammar) (parser parser) &key save-p)
  `(or ,(code (sub-grammar grammar) parser :save-p save-p) t))

(defmethod code ((grammar star-grammar) (parser parser) &key save-p)
  `(not (do () ((not ,(code (sub-grammar grammar) parser :save-p save-p))))))

(defmethod code ((grammar plus-grammar) (parser parser) &key save-p)
  (let ((sub-code (code (sub-grammar grammar) parser :save-p save-p)))
    `(and ,sub-code (not (do () ((not ,sub-code)))))))

(defmethod code ((grammar negative-grammar) (parser parser) &key save-p)
  (declare (ignore save-p))
  `(not ,(code (sub-grammar grammar) parser :save-p 'not)))

(defmethod code ((grammar not-match-grammar) (parser parser) &key save-p)
  "Matches only if the sub-grammar fails to match or, if it does
match, if index hasn't moved up to end-match. (I.e. this can only be
used inside a conjuctive grammar."
  (declare (ignore save-p))
  `(not (and
	 ,(code (sub-grammar grammar) parser :save-p 'not)
         (= index end-match))))

(defmethod code ((grammar build-grammar) (parser parser) &key save-p)
  (declare (ignore save-p))
  (let ((vars (productions-called (grammar grammar))))
    `(let (,@vars)
      (declare (ignorable ,@vars))
      (and
       ,(code (grammar grammar) parser :save-p t)
       (progn ,(form grammar) t)))))

(defmethod code ((grammar token-match-grammar) (parser parser) &key save-p)
  (with-slots (name) grammar
    `(and
      (< index end)
      (let ((token (aref input index)))
        (and
         (eql (kind token) ',name)
         ,@(if save-p `((progn (setf last-match token ,name token) t)))))
      (progn (incf index) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find productions used anywhere in the grammar and any sub-grammars
;; recursively. When generating an all-in-one parser function, each
;; used production is translated into a local function with LABLES.

(defgeneric productions-used (grammar-or-production seen)
  (:documentation "Return a list of productions used by a grammar
  or a gramatical production."))

(defmethod productions-used ((grammar grammar) seen)
  (declare (ignore seen))
  nil)

(defmethod productions-used ((grammar production-call-grammar) seen)
  "A production call uses the called production and any
productions in its own grammar."
  (with-slots (name) grammar
    (unless (gethash name seen)
      (setf (gethash name seen) t)
      (unless (gethash name *productions*)
	(error "No production named: ~a" name))
      (append
       (productions-used (gethash name *productions*) seen)
       (list name)))))

(defmethod productions-used ((grammar sequence-grammar) seen)
  "A sequence grammar uses all the productions its sub-grammars
call."
  (let ((list ()))
    (dolist (g (sub-grammars grammar))
      (dolist (prod (productions-used g seen))
        (push prod list)))
    (nreverse list)))

(defmethod productions-used ((grammar composite-grammar) seen)
  "A composite grammar uses whatever is in its sub-grammar."
  (productions-used (sub-grammar grammar) seen))

(defmethod productions-used ((grammar build-grammar) seen)
  (productions-used (grammar grammar) seen))

(defmethod productions-used ((production chartype-production) seen)
  (declare (ignore seen))
  nil)
  
(defmethod productions-used ((production grammar-production) seen)
  (productions-used (grammar production) seen))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find the productions called directly by a grammar so we know what
;; variables we need to declare in a build-grammar.

(defgeneric productions-called (grammar)
  (:documentation "Return a list of productions called by the grammar"))

(defmethod productions-called ((grammar grammar)) nil)

(defmethod productions-called ((grammar production-call-grammar))
  "A production call uses its production."
  (list (name grammar)))

(defmethod productions-called ((grammar token-match-grammar))
  (list (name grammar)))

(defmethod productions-called ((grammar sequence-grammar))
  "A sequence grammar uses all the productions its sub-grammars
call."
  (let ((list ()))
    (dolist (g (sub-grammars grammar))
      (dolist (prod (productions-called g))
        (pushnew prod list)))
    (nreverse list)))

(defmethod productions-called ((grammar composite-grammar))
  "A composite grammar uses whatever is in its sub-grammar."
  (productions-called (sub-grammar grammar)))

(defmethod productions-called ((grammar negative-grammar)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code generation to stitch it all together.

(defgeneric emit-production-function (production parser)
  (:documentation "Emit code for a function definition that will match
the grammar of a given production"))

(defmethod emit-production-function ((production chartype-production) (parser parser))
  (with-slots (name) production
    `(,name (&key save-p)
      (and
       (< index end)
       (let ((current-char (char input index)))
         (and
          (typep current-char ',name)
          (progn (when save-p (setf last-match current-char)) t)))
       (progn (incf index) t)))))


(defmethod emit-production-function ((production grammar-production) (parser parser))
  (with-slots (name variables grammar) production
    `(,name (&key save-p)
      (declare (ignorable ,@variables))
      (let (,name ,@variables (start index))
        (symbol-macrolet ((result ,name))
          (and ,(code grammar parser)
               (progn
                 (when save-p
                   (setf last-match (or ,name (subseq input start index))))
                 t)))))))

(defgeneric emit-parser-body (parser)
  (:documentation "Emit the whole body of the parser."))

(defmethod emit-parser-body ((parser parser))
  "Emit the body of a parser. Used by both defparser and parselet."
  (with-slots (name grammar) parser
    `(,name (input)
      (let ((output (make-array 0 :adjustable t :fill-pointer t)))
        (let ((index 0) (end (length input)) last-match)
          (declare (ignorable last-match))
          (labels
              (,@(mapcar
                  #'(lambda (p)
                      (emit-production-function
                       (gethash p *productions*)
                       parser))
                  (productions-used grammar (make-hash-table))))
            
            (let (result)
              (values
               (and
                ,(code grammar parser)
                (= index end))
               (if (zerop (length output)) result output)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(defmacro defchartype (name &body typespec)
  "Define a character-type production. The body should be a typespec
that defines a type whose extension is a subset of characters, e.g.
'(member #\a #\b #\c)"
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf (gethash ',name *productions*)
            (make-instance 'chartype-production :name ',name)))
    (deftype ,name () ,@typespec)))

(defmacro defprod (name (&rest vars) &body spec)
  "Define an arbitrary production in the PARSER grammar language."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (gethash ',name *productions*)
     (make-instance 'grammar-production
      :name ',name
      :variables ',vars
      :grammar (make-grammar ',@spec)))))

(defmacro defparser (name spec &key (type 'character-parser))
  "Define a parser function in the PARSER grammar language."
  (let ((parser
         (make-instance type
                        :name name
                        :grammar (make-grammar spec))))
    `(defun ,@(emit-parser-body parser))))

(defmacro deflexer (name spec (&rest meta))
  "Define a parser that collects a vector of tokens."
  (let ((parser
         (make-instance 'character-parser
                        :name name
                        :grammar (make-grammar spec))))
    (dolist (m meta)
      (when (eql (car m) :tokens) (setf (tokens parser) (cdr m))))
    `(defun ,@(emit-parser-body parser))))

(defmacro parselet ((&rest bindings) &body forms)
  "Define local parsers. Parslet is to defparser as flet is to defun."
  `(flet (,@(mapcar
             #'(lambda (binding) 
                 (destructuring-bind
                       (name spec &key (type 'character-parser)) binding
                   (emit-parser-body
                    (make-instance type
                                   :name name
                                   :grammar (make-grammar spec)))))
             bindings))
    ,@forms))


(defun tokenize-file (filename lexer-fn)
  "Tokenize the given file with the given lexer function."
  (with-open-file (in filename)
    (let ((str (make-string (file-length in))))
      (read-sequence str in)
      (funcall lexer-fn str))))

(defun dump-tokens (filename lexer-fn)
  (multiple-value-bind (ok tokens) (tokenize-file filename lexer-fn)
    (declare (ignore ok))
    (loop for tok across tokens do
          (format t "Type: ~A; Value: ~A~&"
                  (kind tok) (value tok)))))

