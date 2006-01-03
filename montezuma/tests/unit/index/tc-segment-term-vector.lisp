(in-package #:montezuma)

(deftestfun test-segment-term-vector
  (let* ((terms (list "Apples" "Oranges" "Bananas" "Kiwis" "Mandarins"))
	 (term-freqs (list 4 2 1 12 4))
	 (stv (make-instance 'segment-term-vector
			     :field "Fruits"
			     :terms terms
			     :term-frequencies term-freqs)))
    (test test-segment-term-vector-1 (size stv) (length terms))
    (test test-segment-term-vector-2 (index-of stv "Apples") 0)
    (test test-segment-term-vector-3 (elt (term-frequencies stv) (index-of stv "Apples")) 4)
    (test test-segment-term-vector-4 (indexes-of stv (list "Bananas" "Apples" "Kiwis") 0 3) '(2 0 3) #'equal)
    (test test-segment-term-vector-5 (indexes-of stv (list "Bananas" "Apples" "Kiwis") 1 2) '(0 3) #'equal)))
