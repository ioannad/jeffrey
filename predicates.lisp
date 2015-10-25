(in-package :jeffrey.predicates)


(defun ancestors (node) ;; => list
  "Returns the list of strict ancestors of NODE."
  (remove-duplicates (apply #'append (node-parents node)
			    (map 'list 
				 #'ancestors 
				 (node-parents node))))
  )

#|
timing of the above>

CL-USER> (time (ancestors (call 'Form0)))
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  28,127 processor cycles
  0 bytes consed

;;; This was previously "implies-p" but it's slower and longer

(defun implies-p (X Y)
  "Returns T if there is a path from X to Y in the transitive 
closure of NODE-PARENTS path from X to Y, 
otherwise returns NIL, which does not mean a non-implication. 
X and Y should be nodes."
  (check-type X node)
  (check-type Y node)
  (cond ((equal X Y)     T) ;; <-- Change to NIL if a strict partial order is necessary.
;;      ((not (node-parents Y)) NIL)    ;; <-- Used to be necessary until I added 'Form1 as a
                                        ;; parent to every parentless node that is not 'Form1.
	((member X (node-parents Y)) T)
	(T (let ((args (map 'list (lambda (Z) (implies-p X Z)) 
			    (node-parents Y))))
	     (eval `(or ,@args))))))
    
;;; Comparison to #'ancestors:

CL-USER> (time (implies-p (call 'Form1) (call 'Form0)))

Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  3,927 processor cycles
  0 bytes consed
  
T

;; so they are similar yet ancestors looks better (and catches loops). So: |#

(defun implies-p (X Y)
  (member X (ancestors Y)))


;;; For implies-not-p I define the descendants of a node.
;;; That is because the (implies-not-p Y X) holds (exactly??) if 
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
				   (node-children node)))))
  )

#| Time>
CL-USER> (time (descendants (call 'Form1)))

Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  50,204 processor cycles
  0 bytes consed
|#

;;; I will also use a predicate that tells me if there is an edge with 
;;; :relation NIL from a Y to an X.

(defun nil-edge? (Y X)  ;; => NIL or nonempty list
  "Returns T if there is an edge in (node-edges Y) with :destination X
and :relation NIL. X and Y must be nodes."
  (some (lambda (edge)
	  (and (eq X (edge-destination edge))
	       (not (edge-relation edge))))
	(node-edges Y)))


		  
(defun implies-not-p (Y X)
  "Returns T if there is a (relation NIL) from X to Y,
or if there is a (relation T) path from Y to Z and from 
W to X, and a (relation NIL) path from W to Z. X and Y
must be nodes."
  (some (lambda (X-desc) 
	  (some (lambda (Y-anc) (nil-edge? Y-anc X-desc))
		(cons Y (ancestors Y))))
	(cons X (descendants X))))

#|
(defun implies-not-p (Y X)
  ;; Cache (ANCESTORS Y)
  (let ((Y+ancestors (list* Y (ancestors Y))))
    (flet ((ancestors-nil-edge-p (x-desc)
	     (some (lambda (Y-anc) (nil-edge? Y-anc X-desc))
		   Y+ancestors)))
      (some #'ancestors-nil-edge-p (list* X (descendants X))))))
|#

#| Time:

CL-USER> (time (implies-not-p (call 'g) (call 'Form1)))
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  93,708 processor cycles
  4,096 bytes consed

|#


