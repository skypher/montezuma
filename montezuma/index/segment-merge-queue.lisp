(in-package #:montezuma)

(defclass segment-merge-queue (priority-queue)
  ()
  (:default-initargs
   :element-type 'segment-merge-info))

(defmethod less-than ((self segment-merge-queue) sti-a sti-b)
  (if (term-buffer= (term-buffer sti-a) (term-buffer sti-b))
      (< (base sti-a) (base sti-b))
      (term-buffer< (term-buffer sti-a) (term-buffer sti-b))))

(defmethod close ((self segment-merge-queue))
  (with-slots (heap size) self
    (dosequence (sti (subseq heap 1 (+ size 1)))
      (when sti
	(close sti)))))
