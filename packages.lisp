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
	   :print-graph))

(defpackage jeffrey.predicates
  (:use :common-lisp
	:jeffrey.graph)
  (:export :ancestors
	   :implies-p
	   :descendants
	   :implies-not-p))


(defpackage jeffrey.read-book1
  (:use :common-lisp
	:mpc :mpc.characters :mpc.numerals
	:jeffrey.graph
	:jeffrey.read)
  (:export :make-graph-from-book1
	   :make-book1))

(defpackage jeffrey.test-book1
  (:use :common-lisp
	:mpc :mpc.characters :mpc.numerals
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates
	:jeffrey.read-book1)
  (:export :test-predicates-with-book1))

(defpackage jeffrey.read-forms
  (:use :common-lisp :split-sequence
	:mpc :mpc.characters :mpc.numerals))


(defpackage jeffrey.test
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates
	:jeffrey.read-book1)
  (:export :*simple-test-data*
	   :test-read
	   :setup-test
	   :test-predicates))

(defpackage jeffrey.draw
  (:use :common-lisp
	:jeffrey.read
	:jeffrey.read-book1)
  (:export :draw-graph))
