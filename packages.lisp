(defpackage jeffrey.graph 
  (:use :common-lisp)
  (:export :make-node
	   :node-edges
	   :node-parents
	   :node-attributes
	   :add-edge
	   :add-parent
	   :make-edge
	   :edge-destination
	   :edge-relation
	   :edge-attributes
	   ))

(defpackage jeffrey.read
  (:use :common-lisp
	:jeffrey.graph)
  (:export :implication-nodes
	   :implications-to-graph
	   :graph-to-implications
	   :*nodes*
	   :call
	   :add-top-bottom
	   :Form1
	   :Form0
	   :format-implications))

(defpackage jeffrey.predicates
  (:use :common-lisp
	:jeffrey.graph)
  (:export :ancestors
	   :implies-p
	   :descendants
	   :implies-not-p))

(defpackage jeffrey.test
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates)
  (:export :*simple-test-data*
	   :test-read
	   :setup-test
	   :test-predicates))

