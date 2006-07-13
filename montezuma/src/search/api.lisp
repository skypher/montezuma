(in-package #:montezuma)

(defgeneric length-norm (similarity field num-terms)
  (:documentation ""))

(defgeneric query-norm (similarity sum-of-squared-weights)
  (:documentation ""))

(defgeneric tf (similarity freq)
  (:documentation "Term frequency"))

(defgeneric sloppy-freq (similarity distance)
  (:documentation ""))

(defgeneric idf-term (similarity term searcher)                 
  (:documentation ""))

(defgeneric idf-phrase (similarity terms searcher)
  (:documentation "Computes a score factor for a phrase.

The default implementation sums the #idf(Term,Searcher) factor for each term in the phrase.

terms:: the terms in the phrase
searcher:: the document collection being searched
Return:: a score factor for the phrase"))

(defgeneric idf (similarity doc-freq num-docs)
  (:documentation ""))

(defgeneric coord (similarity overlap max-overlap)
  (:documentation ""))

(defgeneric hit (similarity)
  (:documentation "Expert: Iterates over matching all documents, yielding the document number and the score."))

(defgeneric hits-up-to (similarity max-docs)
  (:documentation "Expert: Iterates over matching documents in a range.
max:: Do not score documents past this. Default will search all documents avaliable.
returns:: true if more matching documents may remain."))

#+Ignore
;; defined in index's api
(defgeneric doc (similarity))

(defgeneric score (similarity)
  (:documentation "Returns the score for the current document matching the query. Initially invalid, until #next?() is called the first time.
"))

(defgeneric explain-score (similarity reader document-number)
  (:documentation "Returns an explanation of the score for a document. When this method is used, the #next?(), #skip_to(int) and #score(HitCollector) methods should not be used.
doc:: The document number for the explanation.

An explanation of the score computation for the named document.
"))

;;; ---------------------------------------------------------------------------

(defgeneric create-weight (query searcher)
  (:documentation ""))

(defgeneric weight (query searcher)
  (:documentation "Expert: Constructs and initializes a Weight for a top-level query. "))

(defgeneric rewrite (query reader)
  (:documentation " Expert: called to re-write queries into primitive queries."))

(defgeneric compare (query queries)
  (:documentation "Expert: called when re-writing queries under MultiSearcher. Only implemented by derived queries, with no #create_weight() implementatation.

Compares two ScoreDoc objects and returns a result indicating their sort order.

returns:: +-1+ if +i+ should come before +j+
+1+  if +i+ should come after +j+
+0+  if they are equal
"))

(defgeneric extract-terms (qyery terms)
  (:documentation "Expert: adds all terms occuring in this query to the terms set"))

(defgeneric merge-boolean-queries (query queries)
  (:documentation "Expert: merges the clauses of a set of BooleanQuery's into a single BooleanQuery. A utility for use by #combine() implementations."))

(defgeneric similarity-implementation (query searcher)
  (:documentation "Expert: Returns the Similarity implementation to be used for this query.  Subclasses may override this method to specify their own Similarity implementation, perhaps one that delegates through that of the Searcher.  By default the Searcher's Similarity implementation is returned."))


(defgeneric query (weight)
  (:documentation "The query that this concerns. "))

(defgeneric value (weight)
  (:documentation "The weight for this query."))

(defgeneric sum-of-squared-weights (weight)
  (:documentation "The sum of squared weights of contained query clauses. "))

(defgeneric normalize-weight (weight norm)
  (:documentation "Assigns the query normalization factor to this."))

(defgeneric scorer (weight reader)
  (:documentation "Constructs a scorer for this. "))

(defgeneric each-hit (scorer fn))

(defgeneric each-hit-up-to (scorer max-docs fn))

(defgeneric document (disjunction-sum-scorer))
