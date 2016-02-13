(in-package :jeffrey.predicates)


(defun ancestors (node) ;; => list
  "Returns the list of strict ancestors of NODE."
  ;; somehow it stopped working for :FORM1??
  (if (node-parents node) 
      (remove-duplicates (apply #'append (node-parents node)
				(map 'list 
				     #'ancestors 
				     (node-parents node))))
      nil))

(defun graph-implies-p (X Y)
  (member X (ancestors Y)))


;;; For implies-not-p I define the descendants of a node.
;;; That is because the (implies-not-p Y X) holds  if 
;;; there is an edge with (edge-relation edge) = NIL 
;;; from an ancestor-or-equal of Y to a descendant-or-equal of X.

(defun descendants (node)
  "Returns a list of all Ys such that there is a path of 
edges with (edge-relation edge) = T from NODE to Y. NODE must be a node
and the result will not include NODE."
  (flet ((node-children (X) ;;Maybe belongs to graph package?
	   (map 'list #'edge-destination 
		;; Take just the edges with relation T
		(remove-if-not #'edge-relation (node-edges X))))) 
    (remove-duplicates (apply #'append 
			      (node-children node) 
			      (map 'list 
				   #'descendants
				   (node-children node))))))

(defun nil-edge-p (Y X)  ;; => NIL or nonempty list
  "Returns T if there is an edge in (node-edges Y) with :destination X
and :relation NIL. X and Y must be nodes."
  (some (lambda (edge)
	  (and (eq X (edge-destination edge))
	       (not (edge-relation edge))))
	(node-edges Y)))


		  
(defun graph-implies-not-p (Y X)
  "Returns T if there is a (relation NIL) from X to Y,
or if there is a (relation T) path from Y to Z and from 
W to X, and a (relation NIL) path from W to Z. X and Y
must be nodes."
  (some (lambda (X-desc) 
	  (some (lambda (Y-anc) (nil-edge-P Y-anc X-desc))
		(cons Y (ancestors Y))))
	(cons X (descendants X))))

;;; 0 - unknown
;;; 1 - direct implication
;;; 2 - indirect implication
;;; 3 - direct nonimplication
;;; 4 - indirect nonimplication

(defun make-minimal-matrix-from-graph (nodes) ;=> matrix
  "Returns a matrix with the NODES information."
  ;; Goes through the nodes of the nodes,
  (let* ((n      (1+ (hash-table-count nodes))) ;; 423 is missing.
	 (matrix (make-array (list n n) :initial-element NIL)))
    (loop for node-name being the hash-keys of nodes
       using (hash-value node)
       for i = (node-name-to-number node-name)
       unless (equal i 423) ;; ignoring :FORM423.
       do (if (node-edges node) ;; If a node has edges,
	      ;; then for every edge in the node-edges, 
	      (loop for edge in (node-edges node)
		 do (let ((j (node-name-to-number 
			      (gethash (edge-destination edge)
				       (node-names nodes)))))
		      ;; and if its relation is T or if i=j
		      ;; set m-i-j = 1
		      (if (or (edge-relation edge)
			      (equal i j))
			  (setf (aref matrix i j) 1)
			  ;; else the relation will be NIL so
			  ;; set m-i-j = 3.
			(setf (aref matrix i j) 3))))
	      NIL))
    matrix))


;(defvar *predicate-matrix*
;  (let ((book1 (make-book1)))
;    (make-array 

(defun implication-p (node-i node-j matrix nodes graph-predicate boolean code)
  (let* ((i (node-to-number node-i nodes))
	 (j (node-to-number node-j nodes))
	 (cached (aref matrix i j)))
    (if (not (null cached))
	(ecase cached
	  ((1 2)  boolean)
	  ((3 4)  (not boolean))
	  ((0)    nil))
	;; if the matrix has nothing, ask implies-p and update the matrix.
	(when (funcall graph-predicate node-i node-j)
	  (setf (aref matrix i j) code)
	  t))))

(defun implies-p (node-i node-j matrix nodes)
  "Assume that {node-i} and {node-j} have the numbers {i} and {j} 
in their hash-keys in {nodes}. This function first checks if 
{matrix} has a code in row i, column j (say position {m-i-j}), in 
which case it answers according to that code. If the matrix is not 
filled in at position {m-i-j} then this function asks 
{:jeffrey.predicates.implies-p}, and fills m-i-j in {matrix} with
code 2."
  (implication-p node-i node-j matrix nodes 'graph-implies-p t 2))

(defun implies-not-p (node-i node-j matrix nodes)
    "Assume that {node-i} and {node-j} have the numbers {i} and {j} 
in their hash-keys in {nodes}. This function first checks if {matrix}
has a code in row i, column j (say position {m-i-j}), in which case 
it answers according to that code. If the matrix is not filled in at 
position {m-i-j} then this function asks 
{:jeffrey.predicates.implies-not-p}, and fills m-i-j in {matrix} with
code 4."
    (implication-p node-i node-j matrix nodes 'graph-implies-not-p nil 4))
