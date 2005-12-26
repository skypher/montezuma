(in-package #:montezuma)

(defparameter *index-segments-filename* "segments")

(defparameter *index-deletable-filename* "deletable")

(defparameter *index-filename-extensions*
  '("cfs" "fnm" "fdx" "fdt" "tii" "tis" "frq" "prx" "del"
    "tvx" "tvd" "tvf" "tvp"))

(defparameter *index-compound-extensions*
  '("fnm" "frq" "prx" "fdx" "fdt" "tiii" "tis"))

(defparameter *index-vector-extensions*
  '("tvx" "tvd" "tvf"))
