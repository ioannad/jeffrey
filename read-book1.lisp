;;; The functions defined here convert the matrix that is stored in 
;;; book1 of Howard and Rubin's "Consequences of the Axiom of Choice"
;;; into a graph structure. 
;;; Parsing is done with Max Rottenkolber's Monadic Parser Combinator
;;; (mpc) library. 

(in-package :jeffrey.read-book1)


;;; Parsing.

(defun =field ()
  "A field is a code number for the implication 'Form #row' => 'Form-#column',
where #row or #column is the number of the row or column respectively."
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
{Howard-Rubin-data/book1}. The code in position m_ij is the code for
 the implication 'Form i => Form j'."
  (let* ((book1       (read-book1))
	 (n           (length book1))
	 (book-matrix (make-array (list n n) :initial-element NIL)))
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

;;; -----------------------------------------------------------------

(defun make-placeholders-no-423 (last-number nodes)
  "Creates a placeholder for each node to be in NODES, except of
FORM423, which is 'problematic'"
  (loop for i from  0 to last-number 
     unless (equal i 423)
     do (setf (gethash (number-to-node-name i) nodes) (make-node))))

(defun add-edge-parent-no-423 (form-1 form-2 code nodes)
  "If the code of the implication {form-1} => {form-2} is 1, then 
this function adds to the node representing {form-1}, in the hash 
table {nodes}, an edge with destination {form-2} and relation {T},  
and it adds {form-1} to the list of parents of {form-2}. If the code 
is 3, then this function just adds an edge to {form-1} with 
destination {form-2} and relation {NIL}."
  (cond ((equal code 1)
	 ;;debugging
	 ;;(format t "adding T edge from ~a to ~a.~%" form-1 form-2) 
	 (add-edge (gethash form-1 nodes)
		   (make-edge (gethash form-2 nodes) T))
	 ;;debugging
	 ;;(format t "adding ~a as a parent of ~a.~%" form-1 form-2)
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
    ;; First make placeholders for the nodes in the hash table nodes.
    (make-placeholders-no-423 n nodes) 
    ;; Then add the edges and parents, only when book1 has an entry with
    ;; code 1 or 3.
    (loop for i from 0 to n
       unless (equal i 423) ;; problematic form, I ignore it for the moment. 
       do (let ((form-1 (number-to-node-name i)))
	    (loop for j from 0 to n
	       unless (or (equal j 423) ;; see above.
			  (equal j i))
	       do (let ((form-2 (number-to-node-name j))
			(code (aref book1 i j)))
		    (add-edge-parent-no-423 form-1 form-2 code nodes)))))
    nodes))
