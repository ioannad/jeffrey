(in-package :jeffrey.read)

(defun implication-nodes (implications) ;; => '(node-names)
  "Returns a list of all node names mentioned in IMPLICATIONS.
This function is used when importing implication of the shape:
'(form-1 form-2 code references)."
  (remove-duplicates
   (loop for implication in implications
      collect (first implication)
      collect (second implication))))

;; The choiceless lattice has a bottom node, :Form0. I add this
;; manually, because the database doesn't consistently cover this 
;; information, which is of course obvious to a set theorist.
;; (:Form0 ≣ "0=0", i.e., just ZF.)

(defun add-edge-to-Form0 (node nodes)
  "If NODE has no edges and is not :Form0, then add an edge to
NODE one with :destination :Form0 :relation T, and also add NODE 
to the parents of :Form0."
  (let ((node0 (gethash :Form0 nodes)))
    (if (or (some #'edge-relation (node-edges node))
	    (equal node node0))
	nil
	(progn (add-edge node (make-edge node0 t))
	       (add-parent node0 node)
	       (format t "Added edge from ~a to :Form0.~%" node)))))

;; Similarly it has a top element, :Form1, the Axiom of Choice, 
;; which has to be added manually:
(defun add-Form1-as-parent (node nodes)
  "If NODE is not :Form1 and it has no parents then add :Form1 
as a parent and add an edge to :Form1 with destination NODE and 
relation T. NODE must be a node."
  (let ((node1 (gethash :Form1 nodes)))
    (if (or (node-parents node)
	    (eq node node1))
	nil
	(progn (add-parent node node1)
	       (add-edge node1 (make-edge node T))
	       (format t "Added :Form1 as a parent of ~a.~%" node)))))



;; In the following step edges to:Form0 and :Form1 are not added explicitely, 
;; because I plan to use IMPLICATIONS-TO-GRAPH to test if the graph is acyclic. 
;; Don't forget to add these 'Forms when reading the real thing!!!!!!!!!!!!!!!

(defun implications-to-graph (implications)  ;; => HASH-TABLE 
  "Returns a hash-table with the nodes of the graph, placing edges
 and parents appropriately."
  (let ((nodes (make-hash-table))) ;; If you want string keys you need :TEST 'EQUAL
    ;; Create nodes
    (loop for node in (implication-nodes implications) do
	 (setf (gethash node nodes) (make-node)))
    ;; Add edges and parents
    (loop for (from to relation) in implications 
       ;; some attributes of the forms are stored in implications
       ;; of the form (A A "(1)" references), but for now they 
       ;; are just dropped.
       when (not (equal from to)) 
       do
       ;; Some verbocity to trace what is being added in case of an error
	 (format t "Adding ~a -edge from ~a to ~a.~%"
		 relation from to)
       ;; Add edge
	 (add-edge (gethash from nodes)
		   (make-edge (gethash to nodes) relation))
       ;; Add parent if relation is T
	 (when relation
	   (add-parent (gethash to nodes) (gethash from nodes))))
    ;; Return table of nodes
    nodes))

(defun node-names (nodes)    ;; Make  reverse mapping
  (let ((nodes-names (make-hash-table)))
    (maphash (lambda (name node) (setf (gethash node nodes-names) name))
	     nodes)
  nodes-names))

(defun graph-to-implications (nodes)
  "Returns a list of all implications resulting from the graph stored 
in NODES."
  (let ((node-names (make-hash-table)))
    ;; Compile implications
    (loop for name being the hash-keys of nodes
       for node = (gethash name nodes)
       append (loop for edge in (node-edges node)
		 collect `(,name
			   ,(gethash (edge-destination edge) node-names)
			   ,(edge-relation edge))))))

(defun print-graph (nodes)
  "Prints a slightly readable version of the hash table NODES."
  (maphash (lambda (key value) 
	     (format t "Key= ~S ; Value = ~a~%" key value))
	   nodes))


(defun add-top-bottom (nodes)
  "Manually adds :Form1 as top node and :Form0 as bottom node
of the lattice."
  (loop for node being the hash-values in nodes
     do (progn (add-edge-to-Form0 node nodes)
	       (add-Form1-as-parent node nodes)
	       T))
  nodes)



(defvar *nodes* (make-hash-table))

;; The following will be used a lot for testing:
(defun call (node-name)
  "Returns the node in *nodes* with name node-name."
  (gethash node-name *nodes*))


;; And this is meant to convert the old implications format (a list of plists)
;; to the new (from to relation)hh

(defun format-implications (implications) ;;=> list of implications
  "Returns a list of implications of the form '(from to relation). Attributes 
should be added later."
  (loop for implication in implications
     for code   = (getf implication :code)
     for form-1 = (getf implication :form-1)
     for form-2 = (getf implication :form-2)
     when (or (equal code "(1)")
	      (equal code "(3)"))
     collect (list (intern (string-upcase form-1) :keyword)
		   (intern (string-upcase form-2) :keyword)
		   (if (equal code "(1)")
		       T
		       NIL)
		   (list :references
			 (getf implication :references)))))





;;; ---- read-book-1 ------------------------------------


;;; The functions defined here convert the matrix that is stored in
;;; book1 of Howard and Rubin's "Consequences of the Axiom of
;;; Choice" into a graph structure. 
;;; Parsing is done with Max Rottenkolber's Monadic Parser
;;; Combinator (mpc) library. 


;;; Parsing.

(defun =field ()
  "A field is a code number for the implication 'Form #row' => 
'Form-#column', where #row or #column is the number of the row or 
column respectively."
  (=skip-whitespace (=natural-number)))

(defun =row-delim ()
  "Book1 ends rows with a -1"
  (=skip-whitespace (=string "-1")))

(defun =row ()
  "A row is a list of one or more fields that ends in -1."
  (=prog1 (=one-or-more (=field)) (=row-delim)))

(assert (equal '(1 2 3) (run (=row) "1 2 3 -1")))

(defun =book1 ()
  "Book1 is a list of one or more rows."
  (=one-or-more (=row)))

(defun read-book1 ()
  "Parses book1 and returns a list of rows, i.e., a list of lists
of fields." 
  (with-open-file (book1 "Howard-Rubin-data/book1")
    (with-standard-io-syntax (run (=book1) book1))))


(defun make-book1 ()
  "Creates a matrix containing book1, from the file 
{Howard-Rubin-data/book1}. The code in position m_ij is the code 
for the implication 'Form i => Form j'."
  (let* ((book1       (read-book1))
	 (n           (length book1))
	 (book-matrix (make-array (list n n)
				  :initial-element NIL)))
    ;; ensures that the matrix is square
    (assert (equal n (length (first book1))))
    (format t "Book 1 has ~a rows/columns.~%" n)
    (loop for row in book1
       for i from 0 to n
       unless (equal i 423)
       do (loop for code in row
	     for j from 0 to n
	     unless (equal j 423)
	     do (setf (aref book-matrix i j) code))
       finally (return book-matrix))))

;;; Now read the codes (1) and (3) into jeffrey's graph structure 
;;; and use a new version of implies-p and implies-not-p that fills
;;; in the gaps. In the end, check if jeffrey produces the same 
;;; matrix as book1.

;;; n is the amount of forms in the data, i.e., the size of either
;;; dimention of the square matrix book1.

;;; --------------------------------------------------------------

(defun make-placeholders-no-423 (last-number nodes)
  "Creates a placeholder for each node to be in NODES, except of
FORM423, which is 'problematic'"
  (loop for i from  0 to last-number 
     unless (equal i 423)
     do (setf (gethash (number-to-node-name i) nodes)
	      (make-node))))

(defun add-edge-parent-no-423 (form-1 form-2 code nodes)
  "If the code of the implication {form-1} => {form-2} is 1, then 
this function adds to the node representing {form-1}, in the hash 
table {nodes}, an edge with destination {form-2} and relation {T}, 
and it adds {form-1} to the list of parents of {form-2}. If the 
code is 3, then this function just adds an edge to {form-1} with 
destination {form-2} and relation {NIL}."
  (cond ((equal code 1)
	 ;;debugging
	 ;;(format t "adding T edge from ~a to ~a.~%"
	 ;;        form-1 form-2) 
	 (add-edge (gethash form-1 nodes)
		   (make-edge (gethash form-2 nodes) T))
	 ;;debugging
	 ;;(format t "adding ~a as a parent of ~a.~%"
	 ;;        form-1 form-2)
	 (add-parent (gethash form-2 nodes)
		     (gethash form-1 nodes)))
	((equal code 3)
	 (add-edge (gethash form-1 nodes)
		   (make-edge (gethash form-2 nodes) NIL)))
	(T NIL)))

(defun make-graph-from-book1 (book1)
  "*Arguments and values:*

_book1_-a square matrix (array) containing book1

*Description:* 
Creates a graph structure, as defined in {:jeffrey.graph}, from a 
matrix containing book1. Only adds implications that have code 1 
or 3, because implications with codes 2 and 4 are derived from the
ones with codes 1 and 3, and because we ignore implications in ZF-, 
i.e., implications with code 5 or 6. Implications with code 7 will 
be read in a later version, when the references to the implications
will be needed."
  ;;=> hash-table with the nodes.
  (let ((nodes (make-hash-table))
	(n     (1- (first (array-dimensions book1)))))
    ;; First make placeholders for the nodes in nodes.
    (make-placeholders-no-423 n nodes) 
    ;; Then add the edges and parents, only when book1 has an
    ;; entry with code 1 or 3.
    (loop for i from 0 to n
       unless (equal i 423) ;; problematic form, I ignore it for
                            ;; the moment. 
       do (let ((form-1 (number-to-node-name i)))
	    (loop for j from 0 to n
	       unless (or (equal j 423) ;; see above.
			  (equal j i))
	       do (let ((form-2 (number-to-node-name j))
			(code (aref book1 i j)))
		    (add-edge-parent-no-423
		     form-1 form-2 code nodes)))))
    nodes))






;;; --------------------------------- Some helper functions: 

(defun number-to-node-name (k)
  "Takes a number {k} and returns the keyword name of the form 
with number {k}."
  (intern (concatenate 'string "FORM" (write-to-string k))
	  "KEYWORD"))

(defun node-name-to-number (node-name)
  "Takes the keyword {node-name} and returns the number of the
form it describes."
  (parse-integer (subseq (symbol-name node-name) 4)))

(defun node-to-number (node graph)
  "A reverse mapping from a hash value {node} in the hash table 
{graph} to the number in the hash key for {node}, e.g., the node 
with hash value {:FORM14} will be mapped to {14}."
  (node-name-to-number (gethash node (node-names graph))))





