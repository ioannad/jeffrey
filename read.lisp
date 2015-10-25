(in-package :jeffrey.read)

(defun implication-nodes (implications) ;; => '(node-names)
  "Returns a list of all node names mentioned in IMPLICATIONS."
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
    (loop for (from to relation) in implications do
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

(defun graph-to-implications (nodes)
  "Returns a list of all implications resulting from the graph stored 
in NODES."
  (let ((node-names (make-hash-table)))
    ;; Make temporary reverse mapping
    (maphash (lambda (name node) (setf (gethash node node-names) name))
	     nodes)
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



(defvar *nodes*   ;; To hold the nodes.
 nil)

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
     when (and (or (equal code "(1)")
		   (equal code "(3)"))
	       (not (equal form-1 form-2)))
     collect (list (intern (string-upcase form-1) :keyword)
		   (intern (string-upcase form-2) :keyword)
		   (if (equal code "(1)")
		       T
		       NIL)
		   (list :references (getf implication :references))))
  )






;; To test the graph reading facilities, evaluate the following
;; after having loaded "test.lisp":
#|

(assert (null (set-difference *simple-test-data* 
			      (graph-to-implications 
			       (implications-to-graph *simple-test-data*))
			      :test 'equal)))
(assert (null (set-difference (graph-to-implications 
			       (implications-to-graph *simple-test-data*))
			      *simple-test-data*
			      :test 'equal)))

|#