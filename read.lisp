(in-package :jeffrey.read)

(defvar *local-directory* "~/quicklisp/local-projects/jeffrey/")

(defvar *bad-forms* '(423 374 383)
  "These two forms are removed until I figure out how to deal with
them. Form 423 is equivalent to form 374. For 383, FORMSNUM.TEX says:
\"NOTE that 383 and 232 are equivalent. Therefore, 383, and 
[383 A]- [383 C] have become [232 H]-[232 K]\".")

#| 
I will need to iterate over all forms from 0 to 430 that are not in
*bad-forms*. At this point I should add a macro of some sort to make
these iterations shorter to write. 
|#

(defvar *forms-file* (concatenate 'string
				  *local-directory*
				  "Howard-Rubin-data/FORMSNUM.TEX"))

(defvar *book-file* (concatenate 'string
				 *local-directory*
				 "Howard-Rubin-data/book1"))

(defun read-forms-to-graph (forms-file) ;=> hash table (graph)
  "Parses the data in `file` and returns a hash table with the node 
information."
  (let ((forms (collect-forms forms-file))
	(graph (make-hash-table)))
    (loop for form in forms
       for name = (first form)
       unless (member name *bad-forms*)
       do (setf (gethash name graph)
		(apply #'make-node form)))
    graph))

(defun read-book1 (book-file) ;=> list of lists (book1-list)
  "Parses book1 and returns a list of lists of fields."
  (with-open-file (book1 (pathname book-file))
    (with-standard-io-syntax (run (=book1) book1))))

(defun book1-to-matrix (book1-list) ;=> book1-matrix
  "Takes the result of `read-book1`. Returns a matrix version of 
book1."
  (let* ((n           (length book1-list))
	 (book1-matrix (make-array (list n n) :initial-element NIL)))
    (loop for row in book1-list
       for i from 0 to (- n 1)
       unless (member i *bad-forms*)
       do (loop for code in row
	     for j from 0 to (- n 1)
	     unless (member j *bad-forms*)	       
	     do (setf (aref book1-matrix i j) code))
       finally (return book1-matrix))))

(defun add-edge-and-parent (node-a node-b graph)
  "This function adds an edge with destination `node-b` and relation `T`
to `node-a` in `graph`, and it adds `node-a` to the list of parents of `node-b`."
  (add-edge node-a (make-edge node-b T))
  (add-parent node-b node-a))

(defun add-appropriate-edge (node-a node-b code graph)
  "If `code` = 1 and `node-a` is not `node-b`, this adds a T-edge
from `node-a` to `node-b` and `node-a` to the parents of `node-b`.
If `code` = 3, this adds a NIL-edge from `node-a` to `node-b`.
Else NIL."
  (cond ((and (equal code 1)
	      (not (equal node-a node-b)))
	 (add-edge-and-parent node-a node-b graph))
	((equal code 3)
	 (add-edge node-a (make-edge node-b NIL)))
	(T NIL)))

(defun matrix-to-graph (matrix graph) ;=> hash-table (graph)
  "Takes the result of `book1-to-matrix`, or any similar matrix,
and a hash table (`graph`). Returns a hash table, which is `graph` 
with the appropriate `T` and `NIL` edges, which are mentioned in 
`matrix`, added. These edges are the ones with codes 1 and 3 
respectively."
  (loop for name-i being the hash-keys of graph
     using (hash-value node-i)
     do (loop for name-j being the hash-keys of graph
	   using (hash-value node-j)
	   when (member #1=(aref matrix name-i name-j) '(1 3))
	   do (add-appropriate-edge node-i node-j #1# graph)))
  graph)

(defun graph-to-matrix (graph) ;=> matrix
  "Returns a matrix with the NODES information."
  (let ((matrix (make-array '(431 431) :initial-element NIL)))
    (loop for name-1 being the hash-keys of graph
       using (hash-value node-1)
       unless (member name-1 *bad-forms*)
       do (setf (aref matrix name-1 name-1) 1) 
	 (when #1=(node-edges node-1)
	       (loop for edge in #1#
		  for name-2 = (node-name (edge-destination edge))
		  do (if (edge-relation edge)
			 (setf (aref matrix name-1 name-2) 1)
			 (setf (aref matrix name-1 name-2) 3)))))
    matrix))

(defun add-bottom-node (node graph)
  "Unless `node` has edges or it is node 0, add an edge to `node` 
with :destination node-0 :relation T, and also add `node` to the 
parents of node 0, the bottom node of the graph."
  (let ((node-0 (gethash 0 graph)))
    (unless (or (some #'edge-relation (node-edges node))
		(equal node node-0))
      (add-edge-and-parent node node-0 graph))))

(defun add-top-node (node graph)
  "Unless `node` is node-1 or it has parents, add node-1 as a parent 
and add an edge to node-1 with destination `node` and relation T."
  (let ((node-1 (gethash 1 graph))) 
    (unless (or (node-parents node) 
		(equal node node-1)) 
      (add-edge-and-parent node-1 node graph))))

(defun add-top-bottom (graph)
  "Takes a graph (a hash table of nodes). Returns `graph` after
having added node 1 as top node and node 0 as bottom node."
  (loop for node being the hash-values in graph
     do (progn (add-bottom-node node graph)
	       (add-top-node    node graph))
     finally (return graph)))

(defun read-all-data ()
  "Saves all data from `*forms-file*` and `*book-file*` into 
`*graph*`, and returns `book1-matrix` for testing."
  (let ((graph        (read-forms-to-graph *forms-file*))
	(book1-matrix (book1-to-matrix (read-book1 *book-file*))))
    (setf *graph* (matrix-to-graph book1-matrix graph))
    book1-matrix))  




#| The following are utility functions, might remove them later. |#


(defun graph-to-implications (graph) ;=> list of implications
  "Returns a list of all implications resulting from the graph stored 
in `graph`."
    (loop for name being the hash-keys of graph
       using (hash-value node)
       append (loop for edge in (node-edges node)
		 collect `(,name
			   ,(node-name (edge-destination edge))
			   ,(edge-relation edge)))))

(defun print-graph (graph)
  "Prints a slightly readable version of the hash table `graph`."
  (maphash (lambda (key value) 
	     (format t "Key= ~S ; Value = ~a~%" key value))
	   graph))


;; The following will be used a lot for testing:
(defun call (name) (gethash name *graph*))
