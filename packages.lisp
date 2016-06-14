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
	   :edge-attributes)
  (:documentation "graph.lisp contains the graph structure where the information is stored (the types), and the related functions, which are the basic language of the system. There are two types, one type node, which is a name of type natural number, a list of edges of type edge described below, a list of parents of type node, a LaTeX statement of type string, references of type string, and a placeholder for attributes. The other type is edge, which is a destination of type node, a relation (T or NIL), which corresponds to positive and negative implication arrow respectively, and a placeholder for attributes. The nodes are to be stored in the exported hash-table graph."))

(defpackage jeffrey.parse
  (:use    :common-lisp
	   :split-sequence
	   :mpc :mpc.characters :mpc.numerals)
  (:export :collect-forms
	   :=book1)
  (:documentation "parse.lisp contains parsing functions for reading in node (form) information, and for reading book1, the original matrix with all the implication codes. Form information, i.e., name, LaTeX-statement, and references are parsed in a machete-style chopping of the TeX-file Howard-Rubin-data/FORMSNUM.TEX. Implication information is parsed simply, because book1 is a simple integer matrix whose lines terminate with -1. "))

(defpackage jeffrey.read
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.parse
	:mpc :mpc.characters :mpc.numerals)
  (:export :*local-directory*
	   :*bad-forms*     ;; export for testing only
	   :*book-file*     ;; export for testing only
	   :read-book1      ;; export for testing only
	   :matrix-to-graph ;; export for testing only
	   :graph-to-matrix
	   :add-top-bottom  ;; export for testing only
	   :read-all-data
	   :print-graph     ;; export for testing only
	   :call)           ;; export for testing only
  (:documentation "read.lisp contains the functions that read input, and it can be run in its whole with (read-all-data). This function will first store the form data from FORMSNUM.TEX as nodes in *graph*, then add edges and parents to these nodes, following only the direct information from book1. That is, if book1 has code 1 in position (i,j), then it will add an edge to the node with name i (node i) with destination node j and relation T, and it will add node i to the set of parents of node j. If book1 has code 3 in position (i,j), then it will only add an edge to node i with destination node j and relation NIL. All other codes should be derivable from this information, using the predicates in the next module."))

(defpackage jeffrey.predicates
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read)
  (:export :*jeff-matrix*
	   :setup-jeff-matrix
	   :implies-p
	   :implies-not-p)
  (:documentation "predicates.lisp enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. In particular, (implies-p A B) asks whether A is an ancestor of B and (implies-not-p B A) asks whether there is an ancestor B' of B and a descendant A' of A, such that the node B' has an edge with destination A' and relation NIL. Why is the predicate "implies-p" defined like this is clear. For (implies-not-p B A), assume that there is an ancestor B-anc of B and a descendant A-desc of A, such that B-anc does not imply A-desc (the meaning of a NIL-edge from B-anc to A-desc). Then (implies-not-p B A) must be T, i.e., B does not imply A, because otherwise we have the implication chain: B-anc implies B implies A implies A-desc, therefore B-anc implies A-desc, contradiction to the NIL-edge from B-desc to A-desc. "))

(defpackage jeffrey.test
  (:use :common-lisp
	:mpc :mpc.characters :mpc.numerals
      	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates)
  (:export :test-all)
  (:documentation "test.lisp contains test data and testing functions, which should be run after every and any change in the above files. At this moment, only (test-predicates), (test-read), and (test-add-edge) pass, and checking the matrix against book1 has by now resulted in the temporary removal of two forms (374 and 423) which turn out to be equivalent and thus create a loop. This test is not finished yet. :]"))

(defpackage jeffrey.draw
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates
	:split-sequence)
  (:export :draw)
  (:documentation "Draws diagrams with the command `(draw '(a b c d ...) \"filename\")` where `'(a b c d ...)` is a list of natural numbers up to 430, excluding 360, and 423 and 374 for the moment. Requires the database, i.e., *graph* to be loaded and *jeff-matrix* initiated. Normal users please use the :jeffrey.main package."))

(defpackage jeffrey.main
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates
	:jeffrey.draw)
  (:export :main)
  (:documentation "Install this package using quicklisp (installation instructions for quicklisp can be found in https:////www.quicklisp.org//beta//#installation) and git (https:////git-scm.com//book//en//v2//Getting-Started-Installing-Git) as follows:

* Create a folder called `jeffrey` in `quicklisp//local-projects//`,
* Navigate to this folder in a terminal and type `git init` and `git clone git@github.com:ioannad//jeffrey.git`. Alternatively otherwise download the contents of this repository to this folder. 

To produce a diagram, open an SBCL Common Lisp REPL (I haven't tested it yet in other Common-Lisp implementations, please let me know if it works!). Then type in `(ql:quickload \"jeffrey\")` and then type in `(in-package :jeffrey.main)`. Now, to draw the diagram between the forms with Howard-Rubin numbers (HR) a b c d ... use the command `(main \"a b c d ...\")`."))



