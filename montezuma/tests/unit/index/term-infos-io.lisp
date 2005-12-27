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
  (:testfun test-two-field-io
	    (let ((term-dumbly (make-term "word" "dumbly"))
		  (term-dualize (make-term "word" "dualize"))
		  (term-rev-dualize (make-term "reverse" "ezilaud"))
		  (fis (make-instance 'field-infos)))
	      (add-field-info fis "word" :indexed-p T :store-term-vector T)
	      (add-field-info fis "reverse" :indexed-p T :store-term-vector T)
	      (let ((terms (vector))
		    (term-infos (vector))
		    (tiw (make-instance 'term-infos-writer
					:dir (fixture-var 'dir)
					:segment (format nil "~AG" *test-segment*)
					:field-infos fis
					:interval 128)))
		(let ((reversed-words
		       (sort (mapcar #'(lambda (word)
					 (reverse word))
				     *test-dict*)
			     #'string<)))
		  (do ((i 0 (+ i 1))
		       (words reversed-words (cdr words)))
		      ((endp words))
		    (add-term tiw (make-term "reverse" (car words))
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
	      (let ((tir (make-instance 'term-info-reader
					:dir (fixture-var 'dir)
					:segment (format nil "~AG" *test-segment*)
					:field-infos fis)))
		(test term-info-io-1 (size tir) 564)
		(test term-info-io-2 (skip-interval tir) 16)
		(test term-info-io-3
		      (get-terms-position tir
					  (make-term "word" "duvetyne"))
		      561)
		(test term-info-io-4
		      (get-term-info tir term-dualize)
		      (make-instance 'term-info
				     :doc-freq 1
				     :freq-pointer 1005
				     :prox-pointer 1005
				     :skip-offset 0))
		(test term-info-io-5
		      (get-term-info tir term-rev-dualize)
		      (make-instance 'term-info
				     :doc-freq 1
				     :freq-pointer 70
				     :prox-pointer 70
				     :skip-offset 0))))))
