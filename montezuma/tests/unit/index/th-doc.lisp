(in-package #:montezuma)


(defun index-test-helper-make-binary (size)
  (let ((a (make-array size)))
    (dotimes (i size)
      (setf (aref a i) (mod i 256)))
    a))

(defparameter *index-test-helper-binary-data* (index-test-helper-make-binary 256))
(defparameter *index-test-helper-compressed-binary-data* (index-test-helper-make-binary 56))

(defun index-test-helper-prepare-document ()
  (let ((doc (make-instance 'document)))
    (add-field doc (make-field "test_field1" "field one text"
			       :stored T :index :tokenized :store-term-vector NIL))
    (add-field doc (make-field "test_field2" "field field field two text"
			       :stored T :index :tokenized :store-term-vector :with-positions-offsets))
    (add-field doc (make-field "key_field" "keyword"
			       :stored T :index :untokenized))
    (add-field doc (make-field "unindexed_field" "unindexed field text"
			       :stored T :index NIL))
    (add-field doc (make-field "unstored_field1" "unstored field text one"
			       :stored NIL :index :tokenized :store-term-vector NIL))
    (add-field doc (make-field "unstored_field2" "unstored field text two"
			       :stored NIL :index :tokenized :store-term-vector T))
    (add-field doc (make-field "compressed_field" "compressed text"
			       :stored :compress :index :tokenized :store-term-vector T))
    (add-field doc (make-binary-field "binary_field" *index-test-helper-binary-data* T))
    (add-field doc (make-binary-field "compressed_binary_field" *index-test-helper-compressed-binary-data* :compress))
    doc))

(defun index-test-helper-prepare-documents ()
  (let ((data '(("apple" "green")
		("apple" "red")
		("orange" "orange")
		("grape" "green")
		("grape" "purple")
		("mandarin" "orange")
		("peach" "orange")
		("apricot" "orange"))))
    (mapcar #'(lambda (food)
		(let ((doc (make-instance 'document)))
		  (destructuring-bind (name color) food
		    (add-field doc (make-field "name" name
					       :stored T :index :tokenized :store-term-vector :with-positions-offsets))
		    (add-field doc (make-field "color" color
					       :stored T :index :tokenized :store-term-vector :with-positions-offsets))
		    doc)))
	    data)))

(defun index-test-helper-write-document (dir doc &key (segment "test") (analyzer (make-instance 'whitespace-analyzer)) (similarity (make-default-similarity)))
  (let ((writer (make-instance 'document-writer
			       :directory dir
			       :analyzer analyzer
			       :similarity similarity
			       :blahblah 50)))
    (add-document-to-index-writer writer doc)))

(defun index-test-helper-prepare-book-list ()
  (let ((books '(("P.H. Newby" "Something To Answer For" "1969")
		 ("Bernice Rubens" "The Elected Member" "1970")
		 ("V. S. Naipaul" "In a Free State" "1971")
		 ("John Berger" "G" "1972")
		 ("J. G. Farrell" "The Siege of Krishnapur" "1973")
		 ("Stanley Middleton" "Holiday" "1974")
		 ("Nadine Gordimer" "The Conservationist" "1974")
		 ("Ruth Prawer Jhabvala" "Heat and Dust" "1975")
		 ("David Storey" "Saville" "1976")
		 ("Paul Scott" "Staying On" "1977")
		 ("Iris Murdoch" "The Sea" "1978")
		 ("Penelope Fitzgerald" "Offshore" "1979")
		 ("William Golding" "Rites of Passage" "1980")
		 ("Salman Rushdie" "Midnight's Children" "1981")
		 ("Thomas Keneally" "Schindler's Ark" "1982")
		 ("J. M. Coetzee" "Life and Times of Michael K" "1983")
		 ("Anita Brookner" "Hotel du Lac" "1984")
		 ("Keri Hulme" "The Bone People" "1985")
		 ("Kingsley Amis" "The Old Devils" "1986")
		 ("Penelope Lively" "Moon Tiger" "1987")
		 ("Peter Carey" "Oscar and Lucinda" "1988")
		 ("Kazuo Ishiguro" "The Remains of the Day" "1989")
		 ("A. S. Byatt" "Possession" "1990")
		 ("Ben Okri" "The Famished Road" "1991")
		 ("Michael Ondaatje" "The English Patient" "1992")
		 ("Barry Unsworth" "Sacred Hunger" "1992")
		 ("Roddy Doyle" "Paddy Clarke Ha Ha Ha" "1993")
		 ("James Kelman" "How Late It Was How Late" "1994")
		 ("Pat Barker" "The Ghost Road" "1995")
		 ("Graham Swift" "Last Orders" "1996")
		 ("Arundati Roy" "The God of Small Things" "1997")
		 ("Ian McEwan" "Amsterdam" "1998")
		 ("J. M. Coetzee" "Disgrace" "1999")
		 ("Margaret Atwood" "The Blind Assassin" "2000")
		 ("Peter Carey" "True History of the Kelly Gang" "2001")
		 ("Yann Martel" "The Life of Pi" "2002")
		 ("DBC Pierre" "Vernon God Little" "2003"))))
    (mapcar #'(lambda (book)
		(destructuring-bind (author title year) book
		  (let ((doc (make-instance 'document)))
		    (add-field doc (make-field "author" author
					       :stored T :index :tokenized :store-term-vector :with-positions-offsets))
		    (add-field doc (make-field "title" title
					       :stored T :index :tokenized :store-term-vector :with-positions-offsets))
		    (add-field doc (make-field "year" year
					       :stored T :index NIL :store-term-vector NIL))
		    doc)))
	    books)))


(defparameter *index-test-helper-ir-test-doc-count* 64)

(defun index-test-helper-prepare-ir-test-docs ()
  (let ((body "body")
	(title "title")
	(author "author")
	(text "text")
	(year "year")
	(changing-field "changing_field"))
    (let ((docs (make-array *index-test-helper-ir-test-doc-count*)))
      (setf (aref docs 0) (make-instance 'document))
      (let ((d (make-instance 'document)))
	(setf (aref docs 0) d)
	(add-field d (make-field body "Where is Wally"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field changing-field "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector NIL)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 1) d)
	(add-field d (make-field body "Some Random Sentence read"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 2) d)
	(add-field d (make-field body "Some read Random Sentence read"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 3) d)
	(add-field d (make-field title "War And Peace"
				 :stored T :index :untokenized :store-term-vector :with-offsets))
	(add-field d (make-field body "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field author "Leo Tolstoy"
				 :stored T :index :tokenized :store-term-vector :with-positions))
	(add-field d (make-field year "1865"
				 :stored T :index NIL :store-term-vector NIL))
	(add-field d (make-field text "more text which is not stored"
				 :stored NIL :index :tokenized :store-term-vector NIL)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 4) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector NIL)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 5) d)
	(add-field d (make-field body "Here's Wally"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 6) d)
	(add-field d (make-field body "Some Random Sentence read read read read"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      
      (let ((d (make-instance 'document)))
	(setf (aref docs 7) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 8) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 9) d)
	(add-field d (make-field body "read Some Random Sentence read this will be used after unfinished next position read"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 10) d)
	(add-field d (make-field body "Some read Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field changing-field "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector T)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 11) d)
	(add-field d (make-field body "And here too. Well, maybe Not"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 12) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 13) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 14) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 15) d)
	(add-field d (make-field body "Some read Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 16) d)
	(add-field d (make-field body "Some Random read read Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 17) d)
	(add-field d (make-field body "Some Random read Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field changing-field "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector :with-positions)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 18) d)
	(add-field d (make-field body "Wally Wally Wally"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 19) d)
	(add-field d (make-field body "Some Random Sentence"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field changing-field "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector :with-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 20) d)
	(add-field d (make-field body "Wally is where Wally usually likes to go. Wally Mart! Wally likes shopping there for Where's Wally books. Wally likes to read"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field changing-field "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets)))
      (let ((d (make-instance 'document)))
	(setf (aref docs 21) d)
	(add-field d (make-field body "Some Random Sentence read read read and more read read read"
				 :stored T :index :tokenized :store-term-vector :with-positions-offsets))
	(add-field d (make-field changing-field "word3 word4 word1 word2 word1 word3 word4 word1 word3 word3"
				 :stored T :index :tokenized :store-term-vector NIL)))
      (let ((buf ""))
	(dotimes (i 21)
	  (setf buf (concatenate 'string buf "skip ")))
	(loop for i from 22 below *index-test-helper-ir-test-doc-count*
	   do (let ((d (make-instance 'document)))
		(setf (aref docs i) d)
		(setf buf (concatenate 'string buf "skip "))
		(add-field d (make-field text buf
					 :stored NIL :index :tokenized :store-term-vector :with-positions-offsets)))))
      docs)))

(defun index-test-helper-prepare-search-docs ()
  (let ((data '(("20050930" "word1" "cat1/")
		("20051001" "word1 word2 the quick brown fox" "cat1/sub1")
		("20051002" "word1 word3" "cat1/sub1/subsub1")
		("20051003" "word1 word3" "cat1/sub2")
		("20051004" "word1 word2" "cat1/sub2/subsub2")
		("20051005" "word1" "cat2/sub1")
		("20051006" "word1 word3" "cat2/sub1")
		("20051007" "word1" "cat2/sub1")
		("20051008" "word1 word2 word3 the fast brown fox" "cat2/sub1")
		("20051009" "word1" "cat3/sub1")
		("20051010" "word1" "cat3/sub1")
		("20051011" "word1 word3 the quick red fox" "cat3/sub1")
		("20051012" "word1" "cat3/sub1")
		("20051013" "word1" "cat1/sub2")
		("20051014" "word1 word3 the quick hairy fox" "cat1/sub1")
		("20051015" "word1" "cat1/sub2/subsub1")
		("20051016" "word1 the quick fox is brown and hairy and a little red" "cat1/sub1/subsub2")
		("20051017" "word1 the brown fox is quick and red" "cat1/"))))
    (let ((counter 0))
      (mapcar #'(lambda (doc-fields)
		  (let ((doc (make-instance 'document)))
		    (setf (boost doc) (incf counter))
		    (do ((names '("date" "field" "cat") (cdr names))
			 (values doc-fields (cdr values)))
			((endp names))
		      (add-field doc (make-field (car names) (car values)
						 :stored T :index :tokenized :store-term-vector NIL)))
		    doc))
	      data))))
