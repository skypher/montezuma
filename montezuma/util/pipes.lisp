(in-package #:montezuma)

(defmacro make-pipe (head tail)
  "create a pipe by eval'ing head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(defun pipe-tail (pipe)
  "return tail of pipe or list, and destructively update 
   the tail if it is a function."
  ;; This assumes that pipes will never contain functions as values...
  (if (functionp (rest pipe))
    (setf (rest pipe) (funcall (rest pipe)))
    (rest pipe)))

(defun pipe-head (pipe) (first pipe))

(defun pipe-elt (pipe i)
  "ith element of pipe, 0 based."
  (if (= i 0) (pipe-head pipe)
     (pipe-elt (pipe-tail pipe) (- i 1))))

(defconstant empty-pipe nil)

(defun enumerate (pipe &key count key (result pipe))
  "go through all or count elements of pipe,
   possibly applying the key function. "
  (if (or (eq pipe empty-pipe) (eql count 0))
    result
    (progn
      (unless (null key) (funcall key (pipe-head pipe)))
      (enumerate (pipe-tail pipe)
                 :count (if count (1- count))
                 :key key
                 :result result))))

(defun force (pipe)
  (enumerate pipe))

;;; incorrect version-- as in Norvig.
;(defun filter-pipe (predicate pipe)
;  "keep only items in (non-null) pipe satisfying predicate"
;  (if (funcall predicate (head pipe))
;    (make-pipe (head pipe) (filter-pipe predicate (tail pipe)))
;    (filter-pipe predicate (tail pipe))))

(defun filter-pipe (predicate pipe)
  "keep only items in (non-null) pipe satisfying predicate"
     (if (eq pipe empty-pipe)
      empty-pipe
      (let ((head (pipe-head pipe))
            (tail (pipe-tail pipe)))
      (if (funcall predicate head)
        (make-pipe head (filter-pipe predicate tail))
        (filter-pipe predicate tail)))))
               

(defun map-pipe (fn pipe)
  "Map fn over pipe, delaying all but the first fn call,
   collecting results"
  (if (eq pipe empty-pipe)
    empty-pipe
    (make-pipe (funcall fn (pipe-head pipe))
               (map-pipe fn (pipe-tail pipe)))))


(defun map-pipe-filtering (fn pipe &optional filter-test)
  "Map fn over pipe, delaying all but the first fn call,
   collecting results"
  (if (eq pipe empty-pipe)
    empty-pipe
    (let* ((head (pipe-head pipe))
           (tail (pipe-tail pipe))
           (result (funcall fn head)))
      (if (or (and filter-test (funcall filter-test result))
              result)
        (make-pipe result (map-pipe-filtering fn tail filter-test))
        (map-pipe-filtering fn tail filter-test)))))
      
      


(defun append-pipes (pipex pipey)
  "return a pipe that appends two pipes"
  (if (eq pipex empty-pipe)
    pipey
    (make-pipe (pipe-head pipex)
               (append-pipes (pipe-tail pipex) pipey))))

(defun mappend-pipe (fn pipe)
  "lazily map fn over pipe, appending results"
  (if (eq pipe empty-pipe)
    empty-pipe
    (let ((x (funcall fn (pipe-head pipe))))
      (make-pipe (pipe-head x)
                 (append-pipes (pipe-tail x)
                               (mappend-pipe fn (pipe-tail pipe)))))))

(defun mappend-pipe-filtering (fn pipe &optional filter-test)
  "Map fn over pipe, delaying all but the first fn call,
   appending results, filtering along the way"
  (if (eq pipe empty-pipe)
    empty-pipe
    (let* ((head (pipe-head pipe))
           (tail (pipe-tail pipe))
           (result (funcall fn head)))
      (if (or (and filter-test (funcall filter-test result))
              result)
        (make-pipe (pipe-head result)
                   (append-pipes (pipe-tail result)
                                 (mappend-pipe-filtering fn tail filter-test)))
        (mappend-pipe-filtering fn tail filter-test)))))



#|

(defun integers (&optional (start 0) end)
  "a pipe of integers from START to END."
  (if (or (null end) (<= start end))
    (make-pipe start (integers (+ start 1) end))
    nil))

(enumerate (integers 0 5))

          

                 
|#