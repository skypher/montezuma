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
  (do ((sti (queue-pop self) (queue-pop self)))
      ((null sti))
    (close sti)))

