(defpackage jeffrey.graph 
  (:use :common-lisp)
  (:export :*graph*
	   :make-node
	   :node-name
	   :node-edges
	   :node-parents
	   :node-LaTeX
	   :node-references
	   :add-edge
	   :add-parent
	   :make-edge
	   :edge-destination
	   :edge-relation)
  (:documentation
   "Here the graph related common lisp structures are defined. These are the _nodes_ and _edges_ of the graph. The graph itself is stored in a hash table, which in turn is stored in the universal variable `*graph*`. 

A `node` stores the axiom information (name, edges, parents, LaTeX formatted full statement, and possible references (of where this axiom was first found in the literature).

An `edge` stores an implication (A implies B) or a non-implication (A does not imply B). Only the destination (B) and implication status (T for 'implies', NIL for 'does not imply') are stored in the edge, which in turn is stored in the `node-edges` of A.

Apart from the functions described below: {make-node}, {make-edge}, {add-parent}, and {add-edge}, all other exported functions are the standard ones accessing the structures: {node-name}, {node-edges}, {node-parents}, {node-LaTeX}, {node-references}, {edge-destination}, and {edge-relation}."))

(defpackage jeffrey.parse
  (:use    :common-lisp
	   :maxpc :maxpc.digit :maxpc.char)
  (:export :=text-until
	   :=formsnum.tex
	   :=book1
	   :test-formsnum-parsers)
  (:documentation
   "Contains parsing functions for reading {node} (form) 
    information, and for reading book1, the original matrix with 
    all the implication codes.

    The parsers are based on Max's Parser Combinators: 
    [{maxpc}](https://github.com/eugenia/maxpc)"))

(defpackage jeffrey.process-strings
  (:use    :common-lisp
  	   :maxpc :maxpc.digit :maxpc.char)
  (:export :process-forms
	   :search-replace
	   :*comments*)
  (:documentation
   "Contains functions which make the text in any LaTeX-statement 
    LaTeX compatible (the origin is TeX). It's a crude search and 
    replace routine."))

(defpackage jeffrey.read
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.parse
	:jeffrey.process-strings
	:maxpc :maxpc.char :maxpc.digit)
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
	   :descendants
	   :ancestors
	   :implies-p
	   :implies-not-p)
  (:documentation "predicates.lisp enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. In particular, (implies-p A B) asks whether A is an ancestor of B and (implies-not-p B A) asks whether there is an ancestor B' of B and a descendant A' of A, such that the node B' has an edge with destination A' and relation NIL. Why is the predicate \"implies-p\" defined like this is clear. For (implies-not-p B A), assume that there is an ancestor B-anc of B and a descendant A-desc of A, such that B-anc does not imply A-desc (the meaning of a NIL-edge from B-anc to A-desc). Then (implies-not-p B A) must be T, i.e., B does not imply A, because otherwise we have the implication chain: B-anc implies B implies A implies A-desc, therefore B-anc implies A-desc, contradiction to the NIL-edge from B-desc to A-desc. "))

(defpackage jeffrey.test
  (:use :common-lisp
	:maxpc :maxpc.char :maxpc.digit
      	:jeffrey.graph
	:jeffrey.parse
	:jeffrey.process-strings
	:jeffrey.read
	:jeffrey.predicates)
  (:export :test-all)
  (:documentation "test.lisp contains test data and testing functions, which should be run after every and any change in the above files. At this moment, only (test-predicates), (test-read), and (test-add-edge) pass, and checking the matrix against book1 has by now resulted in the temporary removal of two forms (374 and 423) which turn out to be equivalent and thus create a loop. This test is not finished yet. :]"))

(defpackage jeffrey.draw
  (:use :common-lisp
	:external-program
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates
	:split-sequence)
  (:export :draw)
  (:documentation "Draws diagrams with the command `(draw '(a b c d ...) \"filename\")` where `'(a b c d ...)` is a list of natural numbers up to 430, excluding 360, and 423 and 374 for the moment. Requires the database, i.e., *graph* to be loaded and *jeff-matrix* initiated. Normal users please use the :jeffrey.main package."))

(defpackage jeffrey.labelmaker
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:external-program)
  (:documentation "Creates the fancy labels that dot may use. I hope to make this obsolete at some point, and create the labels on the fly."))


(defpackage jeffrey.main
  (:use :common-lisp
	:jeffrey.graph
	:jeffrey.read
	:jeffrey.predicates
	:jeffrey.draw)
  (:export :name-transformer
	   :graph
	   :random-HR-numbers
	   :*local-directory*
	   :*names*
	   :*bad-forms*)
  (:documentation "Install this package using quicklisp (installation instructions for quicklisp can be found in https:////www.quicklisp.org//beta//#installation) and git (https:////git-scm.com//book//en//v2//Getting-Started-Installing-Git) as follows:

* Create a folder called `jeffrey` in `quicklisp//local-projects//`,
* Navigate to this folder in a terminal and type `git init` and `git clone git@github.com:ioannad//jeffrey.git`. Alternatively otherwise download the contents of this repository to this folder. 

To produce a diagram, open a Common Lisp REPL (I have tested it only with SBCL and Clozure CL so far. Please let me know if you test it with other implementations). Then type in `(ql:quickload \"jeffrey\")` and then type in `(in-package :jeffrey.main)`. Now, to draw the diagram between the forms with Howard-Rubin numbers (HR) a b c d ... use the command `(main \"a b c d ...\")`."))

(defpackage jeffrey.web-draw
  (:use :cl :jeffrey.main)
  (:export :web-draw)
  (:documentation "Encodes subsets of nodes in short strings and takes care of concurrency"))

(defpackage jeffrey.parse-web-input
  (:use :cl :hunchentoot :split-sequence
	:jeffrey.main)
  (:export :process-input
	   :input->keywords)
  (:documentation "Parses the user input and accordingly transforms the input names to be used by `:jeffrey.website`."))

(defpackage jeffrey.latex-in-html
  (:use :cl :external-program :html-template
	:jeffrey.graph
	:jeffrey.main)
  (:export :load-nodes-html
	   :get-by-name
	   :select-by-content))

(defpackage jeffrey.website
  (:use :cl :hunchentoot :html-template
	:jeffrey.parse-web-input
	:jeffrey.latex-in-html
	:jeffrey.main)
  (:documentation "Creates the websites using `html_template` and feeds them to a `hunchentoot` server."))

