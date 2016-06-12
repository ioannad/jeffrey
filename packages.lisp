(defpackage jeffrey.graph 
  (:use :common-lisp)
  (:export :*graph*
	   :make-node
	   :node-name
	   :node-edges
	   :node-parents
	   :node-LaTeX
	   :node-references
	   :node-attributes
	   :add-edge
	   :add-parent
	   :make-edge
	   :edge-destination
	   :edge-relation
	   :edge-attributes))

(defpackage jeffrey.parse
  (:use    :common-lisp
	   :split-sequence
	   :mpc :mpc.characters :mpc.numerals)
  (:export :collect-forms
	   :=book1))

(defpackage jeffrey.read
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.parse
	:mpc :mpc.characters :mpc.numerals)
  (:export :*local-directory*
	   :*bad-forms*     ;; for testing only
	   :matrix-to-graph ;; for testing only
	   :add-top-bottom  ;; for testing only
	   :read-all-data
	   :print-graph
	   :call))

(defpackage jeffrey.predicates
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read)
  (:export :*jeff-matrix*
	   :implies-p
	   :implies-not-p
	   :make-minimal-matrix-from-graph))

(defpackage jeffrey.test
  (:use :common-lisp
	:mpc :mpc.characters :mpc.numerals
      	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates)
  (:export :test-all))

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
