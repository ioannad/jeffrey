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
  (check-type name            integer)
  (check-type LaTeX-statement string)
  (make-node% :name name :LaTeX LaTeX-statement :references references))

(defstruct (edge (:constructor make-edge%))
  (destination nil :type node)
  (relation    T   :type symbol)
  (attributes))

(defun make-edge (destination relation)
  (check-type destination node)
  (check-type relation (member t nil))
  (make-edge% :destination destination :relation relation))

(defun add-edge (node edge)
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
