(in-package #:montezuma)

(defclass segment-merge-queue (priority-queue)
  ())

(defmethod less-than ((self segment-merge-queue) sti-a sti-b)
  (if (term-buffer= (term-buffer sti-a) (term-buffer sti-b))
      (< (base sti-a) (base sti-b))
      (term-buffer< (term-buffer sti-a) (term-buffer sti-b))))

(defmethod close ((self segment-merge-queue))
  (with-slots (heap) self
    (dolist (sti heap)
      (when sti
	(close sti)))))
