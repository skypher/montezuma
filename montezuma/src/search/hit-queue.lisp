(in-package #:montezuma)

(defclass hit-queue (priority-queue)
  ())

(defmethod less-than ((queue hit-queue) hit1 hit2)
  (if (= (score hit1) (score hit2))
      (> (doc hit1) (doc hit2))
      (< (score hit1) (score hit2))))
