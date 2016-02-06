(in-package :jeffrey.test-book1)

#|- 
The functions described here serve to test whether the predicates 
implies-p and not-implies-p return the same as book1. The original 
predicates are "optimised" to fill in the matrix without 
recalculating the predicate, if its value is stored already in the 
matrix.  

For this process, I think that it's better timewise to use nodes 
instead of translations from numbers to nodes and to node-names in 
the "new" predicates. For example, when using numbers (the matrix 
indices) with translations to nodes, evaluating
(matrix-implies-p 0 1 *graph*),
doesn't terminate within 10 minutes, whereas 
(implies-not-p (number-to-node 0 *graph*) (number-to-node 1 *graph*)
returns T in under a second.
-|#


#|----------------------------------------------------------
Say the position in a matrix is denoted by m-i-j. E.g.,
(aref matrix i j) = m-i-j
----------------------------------------------------------|#

(defun node-to-number (node graph)
  "A reverse mapping from a hash value {node} in the hash table 
{graph} to the number in the hash key for {node}, e.g., the node 
with hash value {:FORM14} will be mapped to {14}."
  (node-name-to-number (gethash node (node-names graph))))


(defun make-minimal-matrix-from-graph (graph) ;=> matrix
  "Returns a matrix with the GRAPH information."
  ;; Goes through the nodes of the graph,
  (let* ((n      (1+ (hash-table-count graph))) ;; 423 is missing.
	 (matrix (make-array (list n n) :initial-element NIL)))
    (loop for node-name being the hash-keys of graph
       using (hash-value node)
       for i = (node-name-to-number node-name)
       unless (equal i 423) ;; ignoring :FORM423.
       do (if (node-edges node) ;; If a node has edges,
	      ;; then for every edge in the node-edges, 
	      (loop for edge in (node-edges node)
		 do (let ((j (node-name-to-number 
			      (gethash (edge-destination edge) (node-names graph)))))
		      ;; and if its relation is T or if i=j set m-i-j = 1
		      (if (or (edge-relation edge)
			      (equal i j))
			  (setf (aref matrix i j) 1)
			  ;; else the relation will be NIL so set m-i-j = 3.
			(setf (aref matrix i j) 3))))
	      NIL))
    matrix))

(defun m-implies-p (node-i node-j matrix graph)
  "Assume that {node-i} and {node-j} have the numbers {i} and {j} 
in their hash-keys in {graph}. This function first checks if {matrix}
has a code in row i, column j (say position {m-i-j}), in which case 
it answers according to that code. If the matrix is not filled in at 
position {m-i-j} then this function asks 
{:jeffrey.predicates.implies-p}, and fills m-i-j in {matrix} with
code 2."
  (let ((i (node-to-number node-i graph))
	(j (node-to-number node-j graph)))
    (if (aref matrix i j) ;; First check the matrix.
	(cond ((or (equal (aref matrix i j) 1)
		   (equal (aref matrix i j) 2))
	       T)
	      ((or (equal (aref matrix i j) 3)
		   (equal (aref matrix i j) 4)
		   (equal (aref matrix i j) 0))
	       NIL)
	      (T (error "Wrong value at row ~a, column ~a: ~a" i j (aref matrix i j))))
      ;; if the matrix has nothing, ask implies-p and update the matrix.
	(if (implies-p node-i node-j)
	    (progn (setf (aref matrix i j) 2)
		   T)
	    NIL))))

(defun m-implies-not-p (node-i node-j matrix graph)
    "Assume that {node-i} and {node-j} have the numbers {i} and {j} 
in their hash-keys in {graph}. This function first checks if {matrix}
has a code in row i, column j (say position {m-i-j}), in which case 
it answers according to that code. If the matrix is not filled in at 
position {m-i-j} then this function asks 
{:jeffrey.predicates.implies-not-p}, and fills m-i-j in {matrix} with
code 4."
  (let ((i (node-to-number node-i graph))
	(j (node-to-number node-j graph)))
    (if (aref matrix i j) ;; First check the matrix.
	(cond ((or (equal (aref matrix i j) 3)
		   (equal (aref matrix i j) 4))
	       T)
	      ((or (equal (aref matrix i j) 1)
		   (equal (aref matrix i j) 2)
		   (equal (aref matrix i j) 0))
	       NIL)
	      (T (error "Wrong value at row ~a, column ~a: ~a"
			i j (aref matrix i j))))
	;; Otherwise, if the matrix has nothing, recursively ask 
	;; m-implies-not-p while updating the matrix.
	(if (implies-not-p node-i node-j)
	    (progn
	      ;; setf to code 4 because all graph edges (codes 3)
	      ;; should be already in the matrix by now.
	      (setf (aref matrix i j) 4)
	      T)
	    ;; Otherwise return NIL.
	    NIL))))


(defun fill-missing-positions-using-new-predicates (graph matrix)
  "Fills in the {matrix} using the predicates matrix-implies-p and 
matrix-implies-not-p, and the information in the nodes of {graph}.
It returns the filled in matrix."
  (loop for node-i being the hash-values of graph
     using (hash-key node-name-i)
     for i = (node-name-to-number node-name-i)
     unless (equal i 423)       ;; just to be sure
     do (prog1 (format t "~%Row = ~S: Columns = " i)
	  (setf (aref matrix i i) 1)
	  (loop for node-j being the hash-values of graph
	     using (hash-key node-name-j)
	     for j = (node-name-to-number node-name-j)
	     unless (equal j 423)       ;; just to be sure again
	     do (prog1 (format t "~S   " j)
		  (if (aref matrix i j) ;; if m-i-j exists
		      nil               ;; then do nothing,
		      ;; otherwise ask matrix-implies-p and
		      ;; matrix-implies-not-p.
		      (cond ((m-implies-p
			      node-i node-j matrix graph)
			     T)
			    ((m-implies-not-p
			      node-i node-j matrix graph)
			     T)
			    ;; If neither predicate has an answer,
			    ;; then the answer is unknown (code 0). 
			    (T (setf (aref matrix i j) 0))))))))
  ;; Returns the filled in matrix.
  matrix)


(defun matrix-equivalent-to-book1-p (matrix)
  "Tests if {matrix} has the same dimensions as book1, checks if 
fields with codes 1-4 in {matrix} have the same codes in {book1}, 
if fields with code 0 in {matrix} have code 0 or 7 in {book1}, and
ignores fields that have codes 5 and 6 in {book1}."
  (let* ((book1 (make-book1))
	 (n     (1- (first (array-dimensions matrix))))) 
    (format t "The new matrix has dimensions ~a.~%" (array-dimensions matrix))
    (if (equal (array-dimensions matrix) (array-dimensions book1))
	(format t "Matrices have the same dimensions.~%")
	(format t "Matrices do NOT have the same dimensions!~%"))
    ;; Set all code 7 fields in book1 to 0.
    (loop for i from 0 to n
       do (loop for j from 0 to n
	     do (if (equal (aref book1 i j) 7)
		    (setf (aref book1 i j) 0)
		    nil)))
    ;; Returns T if every item in the list of answers to the question whether
    ;; the two matrices have the same code, for each field, is T, unless
    ;; the codes are 5 or 6.
    (eval (cons 'and
		(loop for i from 0 to n
		   unless (equal i 423)
		   return (loop for j from 0 to n
			      collect (if (not (or (equal (aref book1 i j) 5)
						   (equal (aref book1 i j) 6)))
					  (if (equal (aref book1 i j) (aref matrix i j))
					      T
					      (prog2 (format t "In row ~a, column ~a, book1 has code ~a while matrix has code ~a."
							     i j (aref book1 i j) (aref matrix i j))
						  nil))
					  T)))))))

(defun print-comparison-file (book1 matrix)
  "Creates the text file {comparison-to-book1.txt} with a 
field-to-field comparison of the matrices {book1} and {matrix}.
assumes that the matrices are square and have the same dimensions."
  (let ((n (1- (first (array-dimensions book1)))))
    (with-open-file (stream "comparison-to-book1.txt" 
			    :direction :output 
			    :if-exists :supersede)
      (format stream "*Field to field comparison of the matrix book1 from Howard and Rubin's \"Consequences of the Axiom of Choice Project\".* ~%~% Each field is given as a pair, whose first coordinate is the value in book1 and second coordinate the value in the predicate-induced matrix. Each field is marked by \"m-i-j =\", which indicates that that field is in row i and column j, starting the counting from 0. Equivalently, this is field that codes whether form with number i implies the form with number j.~%~%")
      (loop for i from 0 to n
	 do (progn (format stream "Row ~a:   " i )
		   (loop for j from 0 to n
		      do (format stream
				 "m-~a-~a = (~a, ~a)   "
				 i j (aref book1 i j) (aref matrix i j))))))))


(defun test-predicates-with-book1 (create-file?)
  "Temporarily creates a matrix with {book1} and a matrix induced by 
the new predicates, and uses the predicate {matrix-equal-to-book1-p} 
to check if these two matrices are equivalent. If run with {NIL} as 
an argument, this is all it does. If run with {T} as an argument (or 
anything non {NIL}), it also produces a file with a side-to-side 
comparison of each field in the two matrices."
  (let* ((book1  (make-book1))
	 (graph  (make-graph-from-book1 book1))
	 (matrix (fill-missing-positions-using-new-predicates 
		   graph
		   (make-minimal-matrix-from-graph graph))))
    (if (matrix-equivalent-to-book1-p matrix)
	(format t "Matrices equivalent.")
	;; The predicate will throw an error anyway if they are not
	;; equivalent. 
	(format t "Matrices NOT equivalent."))
    (if create-file?
	(prog2 (print-comparison-file book1 matrix)
	    (format t "Comparison file created."))
	nil)))
	  
