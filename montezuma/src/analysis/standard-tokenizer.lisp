(in-package #:montezuma)


(cl-ppcre:define-parse-tree-synonym alpha
    (:CHAR-CLASS (:RANGE #\a #\z) (:RANGE #\A #\Z)))

(cl-ppcre:define-parse-tree-synonym acronym
    (:SEQUENCE alpha "." (:GREEDY-REPETITION 1 NIL (:sequence alpha "."))))

(cl-ppcre:define-parse-tree-synonym acronym-word
    (:sequence :start-anchor acronym :end-anchor))

(cl-ppcre:define-parse-tree-synonym apostrophe
    (:sequence (:greedy-repetition 1 NIL alpha)
	       (:sequence "'" alpha)))

(cl-ppcre:define-parse-tree-synonym apostrophe-word
    (:sequence :start-anchor apostrophe :end-anchor))

(defparameter *acronym-word-regexp* (cl-ppcre:create-scanner '(:sequence acronym-word)))
(defparameter *apostrophe-word-regexp* (cl-ppcre:create-scanner '(:sequence apostrophe-word)))


(defclass standard-tokenizer (regexp-tokenizer)
  ())


(let ((cached-scanner nil))
(defmethod token-regexp ((self standard-tokenizer))
  (if cached-scanner
      cached-scanner
      (setf cached-scanner
	    (let ((alpha "[a-zA-Z]")
		  (p "[_\\/.,-]")
		  (hasdigit "\\w*\\d\\w*"))
	      (let ((re (concatenate 'string
				     alpha "+"
				     "(('" alpha "+)+"
				     "|\\.(" alpha "\\.)+"
				     "|(@|\\&)\\w+([-.]\\w+)*"
				     ")"
				     "|\\w+(([\\-._]\\w+)*\\@\\w+([-.]\\w+)+"
				     "|" p hasdigit "(" p "\\w+" p hasdigit ")*(" p "\\w+)?"
				     "|(\\.\\w+)+"
				     "|)")))
		(cl-ppcre:create-scanner re)))))))


(defparameter *dot-regexp* (cl-ppcre:create-scanner '(:sequence ".")))
(defparameter *apostrophe-s-regexp* (cl-ppcre:create-scanner '(:sequence "'" (:char-class #\s #\S))))

(defmethod normalize ((self standard-tokenizer) str)
  (if (cl-ppcre:scan *acronym-word-regexp* str)
      (cl-ppcre:regex-replace-all *dot-regexp*
				  str
				  "")
      (if (cl-ppcre:scan *apostrophe-word-regexp* str)
	  (cl-ppcre:regex-replace-all *apostrophe-s-regexp*
				      str
				      "")
	  str)))
