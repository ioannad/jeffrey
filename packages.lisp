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
	:jeffrey.graph
	:mpc :mpc.characters :mpc.numerals)
  (:export :implication-nodes
	   :node-names
	   :implications-to-graph
	   :graph-to-implications
	   :*nodes*
	   :call
	   :add-top-bottom
	   :Form1
	   :Form0
	   :format-implications
	   :number-to-node-name
	   :node-name-to-number
	   :print-graph
	   :node-name-to-number
	   :node-names
	   :node-to-number
	   :make-graph-from-book1
	   :*local-directory*
	   :make-book1))

(defpackage jeffrey.predicates
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read)
  (:export :*predicate-matrix*
	   :implies-p
	   :implies-not-p
	   :make-minimal-matrix-from-graph))

(defpackage jeffrey.read-forms
  (:use :common-lisp
	:split-sequence
	:mpc :mpc.characters :mpc.numerals))

(defpackage jeffrey.test
  (:use :common-lisp
	:mpc :mpc.characters :mpc.numerals
      	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates)
  (:export :*simple-test-data*
	   :test-read
	   :setup-test
	   :test-predicates
	   :test-predicates-with-book1
	   :test-all))

(defpackage jeffrey.draw
  (:use :common-lisp
	:jeffrey.read
	:jeffrey.predicates)
  (:export :draw-graph)
  (:documentation "Draws diagrams with information from the matrix book1 and the form infomation stored in {*forms*}."))

(defpackage jeffrey.main
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.draw))
	

(defpackage jeffrey
  (:documentation "Hello!")
  ;; ...
  )
