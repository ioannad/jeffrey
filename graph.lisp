(in-package :jeffrey.graph)

(defstruct (node (:constructor make-node%))
  (edges nil :type list)
  (parents nil :type list)
  (attributes))

(defun make-node ()
  (make-node%))

(defstruct (edge (:constructor make-edge%))
  (destination nil :type node)
  (relation T :type symbol)
  (attributes))

(defun make-edge (destination relation)
  (check-type destination node)
  (check-type relation (member t nil))
  (make-edge% :destination destination :relation relation))

(defun add-edge (node edge)
  (check-type edge edge)
  (assert (not (member edge #1=(node-edges node))) ()
	  "EDGE is already an edge of NODE.")
  (assert (not (and (member #2=(edge-destination edge) (node-parents node))
		    (edge-relation edge))) ()
	  "EDGE-destination ~a is a parent of node ~a." #2# node)
  (push edge #1#))

(defun add-parent (node parent)
  (check-type parent node)
  (assert (not (member parent #1=(node-parents node))) (parent)
	  "~a is already a parent of NODE." parent)
  (push parent #1#))

;; Prevent stack overflow on printing graph structures

;;; I don't understand this any more....:
(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~a edges, ~a parents"
	    (length (node-edges node))
	    (length (node-parents node)))))
