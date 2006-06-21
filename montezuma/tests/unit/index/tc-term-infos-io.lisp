(in-package #:montezuma)


(defparameter *test-dict*
    '("duad" "dual" "dualism" "dualist" "duality" "dualize" "duan"
      "duarchy" "dub" "dubber" "dubbin" "dubbing" "dubiety" "dubiosity"
      "dubious" "dubiously" "dubiousness" "dubitate" "dubitation" "dubnium"
      "dubonnet" "ducal" "ducat" "ducatoon" "duce" "duchess" "duchesse"
      "duchy" "duck" "duckbill" "duckboard" "ducker" "duckie" "ducking"
      "duckling" "duckpin" "duckshove" "duckshover" "ducktail" "duckwalk"
      "duckweed" "ducky" "duct" "ductile" "ductileness" "ductility"
      "ducting" "ductless" "ductule" "ductulus" "ductwork" "dud" "dudder"
      "duddery" "duddie" "duddy" "dude" "dudeen" "dudgeon" "due"
      "duecento" "duel" "dueler" "dueling" "duelist" "dueller" "duelling"
      "duellist" "duello" "duende" "dueness" "duenna" "duennaship" "duet"
      "duette" "duettino" "duettist" "duetto" "duff" "duffel" "duffer"
      "duffle" "dufus" "dug" "dugong" "dugout" "duiker" "duit" "duke"
      "dukedom" "dukeling" "dukery" "dukeship" "dulcamara" "dulcet"
      "dulcian" "dulciana" "dulcification" "dulcify" "dulcimer" "dulcimore"
      "dulcinea" "dulcitone" "dulcorate" "dule" "dulfer" "dulia" "dull"
      "dullard" "dullness" "dullsville" "dully" "dulness" "dulocracy"
      "dulosis" "dulse" "duly" "duma" "dumaist" "dumb" "dumbass"
      "dumbbell" "dumbcane" "dumbfound" "dumbfounder" "dumbhead"
      "dumbledore" "dumbly" "dumbness" "dumbo" "dumbstruck" "dumbwaiter"
      "dumdum" "dumfound" "dummerer" "dummkopf" "dummy" "dumortierite"
      "dump" "dumpbin" "dumpcart" "dumper" "dumpiness" "dumping"
      "dumpling" "dumplings" "dumpsite" "dumpster" "dumpy" "dun" "dunam"
      "dunce" "dunch" "dunder" "dunderhead" "dunderheadedness" "dunderpate"
      "dune" "duneland" "dunfish" "dung" "dungaree" "dungeon" "dungeoner"
      "dungheap" "dunghill" "dungy" "dunite" "duniwassal" "dunk" "dunker"
      "dunlin" "dunnage" "dunnakin" "dunness" "dunnite" "dunnock" "dunny"
      "dunt" "duo" "duodecillion" "duodecimal" "duodecimo" "duodenectomy"
      "duodenum" "duolog" "duologue" "duomo" "duopoly" "duopsony"
      "duotone" "dup" "dupability" "dupatta" "dupe" "duper" "dupery"
      "dupion" "duple" "duplet" "duplex" "duplexer" "duplexity"
      "duplicability" "duplicand" "duplicate" "duplication" "duplicator"
      "duplicature" "duplicitousness" "duplicity" "dupondius" "duppy"
      "dura" "durability" "durable" "durableness" "durably" "dural"
      "duralumin" "duramen" "durance" "duration" "durative" "durbar"
      "dure" "dures" "duress" "durgan" "durian" "durion" "durmast"
      "durn" "durned" "duro" "duroc" "durometer" "durr" "durra" "durrie"
      "durukuli" "durum" "durzi" "dusk" "duskiness" "dusky" "dust"
      "dustbin" "dustcart" "dustcloth" "dustcover" "duster" "dustheap"
      "dustiness" "dusting" "dustless" "dustman" "dustmop" "dustoff"
      "dustpan" "dustpanful" "dustrag" "dustsheet" "dustup" "dusty"
      "dutch" "dutchman" "duteous" "duteously" "duteousness" "dutiability"
      "dutiable" "dutifulness" "duty" "duumvir" "duumvirate" "duvet"
      "duvetine" "duvetyn" "duvetyne" "dux" "duyker"))

(defparameter *test-segment* "_test")


(deftestfixture term-infos-io
  (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-term-infos-two-field-io
	    (let ((term-dualize (make-term "word" "dualize"))
		  (term-rev-dualize (make-term "reverse" "ezilaud"))
		  (fis (make-instance 'field-infos)))
	      (add-field-info fis "word" :indexed-p T :store-term-vector T)
	      (add-field-info fis "reverse" :indexed-p T :store-term-vector T)
	      (let ((tiw (make-instance 'term-infos-writer
					:directory (fixture-var 'dir)
					:segment (format nil "~AG" *test-segment*)
					:field-infos fis
					:interval 128)))
		(let ((reversed-words
		       (cl:sort (mapcar #'(lambda (word)
					    (reverse word))
					*test-dict*)
				#'string<)))
		  (do ((i 0 (+ i 1))
		       (words reversed-words (cdr words)))
		      ((endp words))
		    (add-term tiw
			      (make-term "reverse" (car words))
			      (make-instance 'term-info
					     :doc-freq 1
					     :freq-pointer i
					     :prox-pointer i
					     :skip-offset 0)))

		  (do ((i 0 (+ i 1))
		       (words *test-dict* (cdr words)))
		      ((endp words))
		    (add-term tiw (make-term "word" (car words))
			      (make-instance 'term-info
					     :doc-freq 1
					     :freq-pointer (+ 1000 i)
					     :prox-pointer (+ 1000 i)
					     :skip-offset 0)))
		  (close tiw)))
	      (let ((tir (make-instance 'term-infos-reader
					:directory (fixture-var 'dir)
					:segment (format nil "~AG" *test-segment*)
					:field-infos fis)))
		(test term-info-two-field-io-1 (size tir) 564)
		(test term-info-two-field-io-2 (skip-interval tir) 16)
		(test term-info-two-field-io-3
		      (get-terms-position tir
					  (make-term "word" "duvetyne"))
		      561)
		(test term-info-two-field-io-4
		      (get-term-info tir term-dualize)
		      (make-instance 'term-info
				     :doc-freq 1
				     :freq-pointer 1005
				     :prox-pointer 1005
				     :skip-offset 0)
		      #'term-info=)
		(test term-info-two-field-io-5
		      (get-term-info tir term-rev-dualize)
		      (make-instance 'term-info
				     :doc-freq 1
				     :freq-pointer 70
				     :prox-pointer 70
				     :skip-offset 0)
		      #'term-info=))))
  (:testfun test-term-infos-io-small
	    (let ((fis (make-instance 'field-infos))
		  (terms '())
		  (term-infos '()))
	      (add-field-info fis "word" :indexed-p T :store-term-vector T)
	      (let ((tiw (make-instance 'term-infos-writer
					:directory (fixture-var 'dir)
					:segment "tiny-test-segment"
					:field-infos fis
					:interval 128)))
		(do ((i 0 (+ i 1))
		     (words (subseq *test-dict* 0 5) (cdr words)))
		    ((endp words))
		  (let ((term (make-term "word" (car words)))
			(term-info (make-instance 'term-info
						  :doc-freq 1
						  :freq-pointer i
						  :prox-pointer i
						  :skip-offset 0)))
		    (push term terms)
		    (push term-info term-infos)
		    (add-term tiw term term-info)))
		(close tiw))
	      (let ((tir (make-instance 'term-infos-reader
					:directory (fixture-var 'dir)
					:segment "tiny-test-segment"
					:field-infos fis)))
		(test term-info-io-small-1 (size tir) 5)
		(test term-info-io-small-2 (skip-interval tir) 16)
		(test term-info-io-small-3 (get-terms-position tir (make-term "word" "duad")) 0)
		(test term-info-io-small-4 (get-terms-position tir (make-term "word" "dual")) 1)
		(test term-info-io-small-5 (get-terms-position tir (make-term "word" "dualist")) 3))))
  (:testfun test-term-infos-io-big
	    (let ((term-dumbly (make-term "word" "dumbly"))
		  (term-dualize (make-term "word" "dualize"))
		  (fis (make-instance 'field-infos))
		  (terms '())
		  (term-infos '()))
	      (add-field-info fis "word" :indexed-p T :store-term-vector T)
	      (let ((tiw (make-instance 'term-infos-writer
					:directory (fixture-var 'dir)
					:segment "tiny-test-segment"
					:field-infos fis
					:interval 128)))
		(do ((i 0 (+ i 1))
		     (words *test-dict* (cdr words)))
		    ((endp words))
		  (let ((term (make-term "word" (car words)))
			(term-info (make-instance 'term-info
						  :doc-freq 1
						  :freq-pointer i
						  :prox-pointer i
						  :skip-offset 0)))
		    (push term terms)
		    (push term-info term-infos)
		    (add-term tiw term term-info)))
		(close tiw))
	      (let ((tir (make-instance 'term-infos-reader
					:directory (fixture-var 'dir)
					:segment "tiny-test-segment"
					:field-infos fis)))
		(test term-info-io-big-1 (size tir) 282)
		(test term-info-io-big-2 (skip-interval tir) 16)
		(test term-info-io-big-3 (get-terms-position tir (make-term "word" "duyker")) 281)
		(test term-info-io-big-4 (get-terms-position tir (make-term "word" "duvetyne")) 279)
		(test term-info-io-big-5 (get-terms-position tir (make-term "word" "dusting")) 254)
		(test term-info-io-big-6 (get-terms-position tir (make-term "word" "dustless")) 255)
		(test term-info-io-big-7 (get-terms-position tir (make-term "word" "dustman")) 256)
		(test term-info-io-big-8 (get-terms-position tir (make-term "word" "dustmop")) 257)
		(test term-info-io-big-9
		      (get-term-info tir term-dualize)
		      (make-instance 'term-info
				     :doc-freq 1
				     :freq-pointer 5
				     :prox-pointer 5
				     :skip-offset 0)
		      #'term-info=)
		(test term-info-big-10 (get-term tir 127) term-dumbly #'term=)
		(let ((terms (terms-from tir term-dumbly)))
		  (test term-info-big-11 (term terms) term-dumbly #'term=)
		  (test term-info-big-12 (and (next? terms) T) T)
		  (test term-info-big-13 (term terms) (make-term "word" "dumbness") #'term=)
		  (test term-info-big-14 (and (next? terms) T) T)
		  (test term-info-big-15 (term terms) (make-term "word" "dumbo") #'term=)))))
  (:testfun test-term-infos-io-small-writer
	  (let ((fis (make-instance 'field-infos)))
	    (add-field-info fis "author" :indexed-p T :store-term-vector T)
	    (add-field-info fis "title" :indexed-p T :store-term-vector T)
	    (let ((tiw (make-instance 'term-infos-writer
				      :directory (fixture-var 'dir)
				      :segment *test-segment*
				      :field-infos fis
				      :interval 128)))
	      (let ((terms (list (make-term "author" "Martel")
				 (make-term "title" "Life of Pi")
				 (make-term "author" "Martin")
				 (make-term "title" "Life on the edge"))))
		(setf terms (cl:sort terms #'term<))
		(let ((term-infos '()))
		  (dotimes (i 4)
		    (push (make-instance 'term-info
					 :doc-freq i
					 :freq-pointer i
					 :prox-pointer i
					 :skip-offset i)
			  term-infos))
		  (setf term-infos (nreverse term-infos))
		  (dotimes (i 4)
		    (add-term tiw (elt terms i) (elt term-infos i)))
		  (close tiw))))
	    (let ((tis-file (open-input (fixture-var 'dir) (format nil "~A.tis" *test-segment*)))
		  (tii-file (open-input (fixture-var 'dir) (format nil "~A.tii" *test-segment*))))
	      (test term-infos-small-writer-1 (read-int tis-file) +term-infos-format+)
	      (test term-infos-small-writer-2 (read-long tis-file)   4)      ;; term count
	      (test term-infos-small-writer-3 (read-int tis-file)  128)      ;; index interval
	      (test term-infos-small-writer-4 (read-int tis-file)   16)      ;; skip interval
	      (test term-infos-small-writer-5 (read-vint tis-file)   0)      ;; string-equal length
	      (test term-infos-small-writer-6 (read-vint tis-file)   6)      ;; rest of string length
	      (let ((author (make-array 6)))
		(read-chars tis-file author 0 6)
		(test term-infos-small-writer-7 (bytes-to-string author) "Martel" #'string=))
	      (test term-infos-small-writer-8 (read-vint tis-file)   0)      ;; field number
	      (test term-infos-small-writer-9 (read-vint tis-file)   0)      ;; doc-freq
	      (test term-infos-small-writer-10 (read-vlong tis-file) 0)      ;; freq pointer difference
	      (test term-infos-small-writer-11 (read-vlong tis-file) 0)      ;; prox pointer difference
	      (test term-infos-small-writer-12 (read-vint tis-file)  4)      ;; string-equal length
	      (test term-infos-small-writer-13 (read-vint tis-file)  2)      ;; rest of string length
	      (let ((author (make-array 2)))
		(read-chars tis-file author 0 2)
		(test term-infos-small-writer-14 (bytes-to-string author) "in" #'string=))
	      (test term-infos-small-writer-15 (read-vint tis-file)  0)      ;; field number
	      (test term-infos-small-writer-16 (read-vint tis-file)  1)      ;; doc-freq
	      (test term-infos-small-writer-17 (read-vlong tis-file) 1)      ;; freq pointer difference
	      (test term-infos-small-writer-18 (read-vlong tis-file) 1)      ;; prox pointer difference
	      (test term-infos-small-writer-19 (read-vint tis-file)  0)      ;; string-equal length
	      (test term-infos-small-writer-20 (read-vint tis-file) 10)      ;; rest of string length
	      (let ((title (make-array 10)))
		(read-chars tis-file title 0 10)
		(test term-infos-small-writer-21 (bytes-to-string title) "Life of Pi" #'string=))
	      (test term-infos-small-writer-22 (read-vint tis-file)  1)      ;; field number
	      (test term-infos-small-writer-23 (read-vint tis-file)  2)      ;; doc-freq
	      (test term-infos-small-writer-24 (read-vlong tis-file) 1)      ;; freq pointer difference
	      (test term-infos-small-writer-25 (read-vlong tis-file) 1)      ;; prox pointer difference
	      (test term-infos-small-writer-26 (read-vint tis-file)  6)      ;; string-equal length
	      (test term-infos-small-writer-27 (read-vint tis-file) 10)      ;; rest of string length
	      (let ((title (make-array 10)))
		(read-chars tis-file title 0 10)
		(test term-infos-small-writer-28 (bytes-to-string title) "n the edge" #'string=))
	      (test term-infos-small-writer-29 (read-int tii-file) +term-infos-format+)
	      (test term-infos-small-writer-30 (read-long tii-file)   1)
	      (test term-infos-small-writer-31 (read-int tii-file)  128)
	      (test term-infos-small-writer-32 (read-int tii-file)   16)
	      (test term-infos-small-writer-33 (read-vint tii-file)   0)     ;; string-equal length
	      (test term-infos-small-writer-34 (read-vint tii-file)   0)     ;; rest of string length
	      (test term-infos-small-writer-35 (read-vint tii-file) #xFFFFFFFF) ;; field number
	      (test term-infos-small-writer-36 (read-vint tii-file)   0)     ;; doc freq
	      (test term-infos-small-writer-37 (read-vlong tii-file)  0)     ;; freq pointer difference
	      (test term-infos-small-writer-38 (read-vlong tii-file)  0)     ;; prox pointer difference
	      (test term-infos-small-writer-39 (read-vlong tii-file) 20))))) ;; pointer to first element in other


	      

	      
	      
	      
	      
