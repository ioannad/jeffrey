(in-package :jeffrey.graph)

(defvar *graph* (make-hash-table :test 'equal)
  "The graph structure may be saved in this universal variable.")

;; The graph structure consists of nodes, whose names have type
;; integer, though they should be natural numbers.

(defstruct (node (:constructor make-node%))
  (name       1000  :type integer) ; A node must have a name. 1000 should not actually occur.
  (edges      nil   :type list)   
  (parents    nil   :type list)
  (LaTeX      ""    :type string)
  (references ""    :type string) 
  (attributes))

(defun make-node (name LaTeX-statement references)
  "*Arguments and values:
   
   _name_-an integer name to be used as a key in the hash table stored in 
   {*graph*}.

   _LaTeX-statement_-A string in LaTeX format, which is the full mathematical 
   statement of the axiom stored in {node}. 

   _references_-A string in LaTeX format, which contains references to where 
   this axiom was studied.

   *Description:*
   
   {make-node} creates a {node} structure with only the slots {name}, {LaTeX}, 
   and {references}. This function type checks these slots. It is used in 
   {read.lisp} to store the axiom information, which is parsed from a TeX file."
  (check-type name            integer)
  (check-type LaTeX-statement string)
  (check-type references      string)
  (make-node% :name name :LaTeX LaTeX-statement :references references))

(defstruct (edge (:constructor make-edge%))
  (destination nil :type node)
  (relation    T   :type symbol)
  (attributes))

(defun make-edge (destination relation)
  "*Arguments and values:*

   _destination_-a {node}
   
   _relation_-{T} for 'implies', or {NIL} for 'does not imply'.
  
   *Description:*

   {make-edge} creates an {edge} structure with the slots ({destination} 
   and {relation}) filled, after type checking the content of the slots. 
   These edges are stored in the origin node as follows.
   
   If we know that a node A implies a node B, then we add an edge E in its 
   {node-edges} list, where E has relation {T} and destination {B}.

   If we know that a node A does not imply a node B, then we add an edge E in 
   its {node-edges} list, where E has relation {NIL} and destination {B}."
  (check-type destination node)
  (check-type relation (member t nil))
  (make-edge% :destination destination :relation relation))

(defun add-edge (node edge)
  "*Arguments and values:*
   
   _node_-a node
   
   _edge_-an edge

  *Description:*

  Adds _edge_ to the list {(node-edges _node_)}, after type checking _edge_,
  and after asserting that this edge is not already in the list, and that 
  {(edge-destination _edge_)} is not a parent of _node_."
  (check-type edge edge)
  (assert (not (member edge #1=(node-edges node))) ()
	  "EDGE ~a is already an edge of NODE ~a." edge node)
  (assert (not (and (member #2=(edge-destination edge)
			    (node-parents node))
		    (edge-relation edge))) ()
	  "EDGE-destination ~a is a parent of node ~a."
	  #2# node)
  (push edge #1#))

(defun add-parent (node parent)
  "*Arguments and values:*
 
   _node_-a node

   _parent_-a node

   *Description:*

   After type checking the arguments, it asserts that _parent_ is not already
   in {(node-parents _node_)} and that _parent_ is not equal to _node_. 
   Then it pushes _parent_ to the list {(node-parents _node_)}."
  (check-type parent node)
  (check-type node   node)
  (assert (not (member parent #1=(node-parents node))) (parent)
	  "~a is already a parent of ~a." parent node)
  (assert (not (equal node parent)) (parent)
	  "Parent and node are the same: ~a." parent)
  (push parent #1#))

;; Prevent stack overflow on printing graph structures

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~a: ~a edges, ~a parents "
	    (node-name node)
	    (length (node-edges node))
	    (length (node-parents node)))))
