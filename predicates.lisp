(in-package :jeffrey.predicates)

#| 
# predicates.lisp

This package contains the predicates that, given the code 1 and 
code 3 implications from book1, can decide whether an arbitrary 
form A implies another B, or not implies B, or that it is unknown 
whether it implies B.

## (graph-implies-p A B)

if there is a path of `:relation T` edges from A to B, i.e., if A 
is an ancestor of B.

## (graph-implies-not-p A B)

if there is a `:relation NIL` edge (a nil-edge) from B or from an 
ancestor of B to A or to a descendant of A. This is because 
otherwise, if A did imply B, then A-ancestor implies A implies B 
implies B-descendant, therefore A-ancestor implies B-ancestor, 
which is a contradiction to the nil-edge.

### Implementation

The easiest way to implement `(graph-implies-p A B)` is to check if
A is a member of the set of ancestors of B. The easiest way to 
implement `(graph-implies-not-p B A) is to check if there is a 
nil-edge from an ancestor-or-equal of A to a descendant-or-equal to
B. The latter is actually a double loop, which is inefficient. For
this reason I use the matrix `*jeff-matrix*` to store the answers 
of `graph-implies-p` and `graph-implies-not-p`. 

A small improvement (about 7.1% in SBCL and 26.5% in CCL) was achieved by changing `(graph-implies-not-p B A)` to first collect the destinations of all nil-edges from ancestors of B, and checking 
whether this intesects the descendants of A. 
|#

(defvar *jeff-matrix* (make-array '(430 430) :initial-element NIL)
  "This matrix is meant to store the answers of the predicates 
`implies-p` and `implies-not-p`.")

(defun setup-jeff-matrix (graph)
  "Prepares *jeff-matrix* with the information from `graph`, and some
obvious predicate answers."
  (setf *jeff-matrix* (graph-to-matrix graph))
  (loop for name being the hash-keys of graph
     do (setf (aref *jeff-matrix* name 0) 1)
       (setf (aref *jeff-matrix* 1 name) 1)))	

(defun ancestors (node) ;; => list
  "Returns the list of strict ancestors of NODE."
  (when (node-parents node) 
    (remove-duplicates 
     (apply #'append
	    (node-parents node)
	    (map 'list #'ancestors (node-parents node))))))

(defun graph-implies-p (X Y)
  (member X (cons Y (ancestors Y))))

(defun descendants (node)
  "Returns a list of all Y such that there is a path of edges with 
`(edge-relation edge) = T` from `node` to Y. The result will not include
`node`."
  (flet ((node-children (X) ;;Maybe belongs to graph package?
	   (map 'list
                #'edge-destination 
		;; Take just the edges with relation T
		(remove-if-not #'edge-relation (node-edges X)))))
    (remove-duplicates
     (apply #'append 
	    (node-children node) 
	    (map 'list #'descendants (node-children node))))))

(defun nil-edge-p (Y X)  ;; => NIL or nonempty list
  "Returns T if there is an edge in (node-edges Y) with :destination X
and :relation NIL."
  (some (lambda (edge)
	  (and (eq X (edge-destination edge))
	       (not (edge-relation edge))))
	(node-edges Y)))

(defun graph-implies-not-p (Y X)
  "Returns T if there is a nil-edge from Y to X, or if there
is an ancestor Y-anc of Y, with a nil-edge to a descendant X-desc 
of X."
  (let ((nil-dests (loop for anc in (cons Y (ancestors Y))
		      append (loop for edge in (node-edges anc)
				if (not (edge-relation edge))
				collect (edge-destination edge)))))
    (intersection nil-dests (cons X (descendants X)))))


;;; 0 - unknown
;;; 1 - direct implication
;;; 2 - indirect implication
;;; 3 - direct nonimplication
;;; 4 - indirect nonimplication

(defun implication-p (node-i node-j graph-predicate boolean code)
  (let* ((name-i (node-name node-i))
	 (name-j (node-name node-j))
	 (cached (aref *jeff-matrix* name-i name-j)))
    (if (not (null cached))
	(ecase cached
	  ((1 2)  boolean)
	  ((3 4)  (not boolean))
	  ((0)    nil))
	(when (funcall graph-predicate node-i node-j)
	  (setf (aref *jeff-matrix* name-i name-j) code)
	  t))))

(defun implies-p (node-i node-j)
  "This takes two nodes with names say i and j, checks if 
*jeff-matrix* is filled in at the field (i,j), in which case it 
answers according to that code. Otherwise it asks `graph-implies-p`,
and if the answer is `T`, it fills (i,j) with code 2, otherwise does
nothing."
  (implication-p node-i node-j 'graph-implies-p t 2))

(defun implies-not-p (node-i node-j)
    "Similarly, this takes two nodes with names say i and j, checks 
if *jeff-matrix* is filled in at the field (i,j), in which case it 
answers according to that code. Otherwise it asks 
`graph-implies-not-p`, and if the answer is `T`, it fills (i,j) with 
code 2, otherwise does nothing."
    (implication-p node-i node-j 'graph-implies-not-p nil 4))
