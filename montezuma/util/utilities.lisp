(in-package montezuma)

(declaim (inline length-1-list-p))
(defun length-1-list-p (x) 
  "Is x a list of length 1? Note that this is better than the naive \(= \(length x\) 1\) because we don't need to traverse the entire list..."
  (and (consp x) (null (cdr x))))