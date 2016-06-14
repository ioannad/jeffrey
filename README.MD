# Choiceless grapher 

This is the early stage of a program which creates diagrams with the relationships between consequences of the Axiom of Choice. This project is inspired by the encyclopedia of Set Theory without the Axiom of Choice:
                          **Consequences of the Axiom of Choice Project**
by Prof. Paul Howard and Prof. Jean E. Rubin. I thank Paul Howard for providing me with the implication matrix (book1), a tex document with the form statements in LaTeX form, and permision to use these files, which can be found in the folder "Howard-Rubin-data".

A quick overview is given below. A paper with a full description and explanation of the code will appear after I upload the diagram drawing part of this program. A big *thank you* to my teammate Max Rottenkolber over at [interstellar ventures](http://inters.co). 

## Installation
Install this package using quicklisp ([installation instructions](https://www.quicklisp.org/beta/#installation)) and git ([installation instructions](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)) as follows:

* Create a folder called `jeffrey` in `quicklisp/local-projects/`,
* Navigate to this folder in a terminal and type `git init` and `git clone git@github.com:ioannad/jeffrey.git`. Alternatively otherwise download the contents of this repository to this folder. 

To produce a diagram, open an SBCL Common Lisp REPL (I haven't tested it yet in other Common-Lisp implementations, please let me know if it works!). Then type in `(ql:quickload "jeffrey")`. Then choose a package you'd like to use, for example to run all tests and read the report type in `(in-package :jeffrey.test)` and then type in `(test-all)`.

## Description
This program requires package "mpc" and "split-sequence". In the to-do list is to change mpc to maxpc, the new version of mpc.

**jeffrey.asd** contains the (defsystem ...) command that creates "jeffrey" as a system of packages. The files that comprise jeffrey's packages, their exported functions, and their dependencies are listed in **packages.lisp**.

**graph.lisp** contains the graph structure where the information is stored (the types), and the related functions, which are the basic language of the system. There are two types, one type `node`, which is a name of type natural number, a list of edges of type edge described below, a list of parents of type node, a LaTeX statement of type string, references of type string, and a placeholder for attributes. The other type is `edge`, which is a destination of type node, a relation (T or NIL), which corresponds to positive and negative implication arrow respectively, and a placeholder for attributes. The nodes are to be stored in the exported hash-table `\*graph\*`.

**parse.lisp** contains parsing functions for reading in node (form) information, and for reading `book1`, the original matrix with all the implication codes. Form information, i.e., name, LaTeX-statement, and references are parsed in a machete-style chopping of the TeX-file `Howard-Rubin-data/FORMSNUM.TEX`. Implication information is parsed simply, because book1 is a simple integer matrix whose lines terminate with -1. 

**read.lisp** contains the functions that read input, and it can be run in its whole with `(read-all-data)`. This function will first store the form data from `FORMSNUM.TEX` as nodes in `\*graph\*`, then add edges and parents to these nodes, following only the direct information from `book1`. That is, if book1 has code 1 in position (i,j), then it will add an edge to the node with name i (node i) with destination node j and relation T, and it will add node i to the set of parents of node j. If book1 has code 3 in position (i,j), then it will only add an edge to node i with destination node j and relation NIL. All other codes should be derivable from this information, using the predicates in the next module.

**predicates.lisp** enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. In particular, `(implies-p A B)` asks whether A is an ancestor of B and `(implies-not-p B A)` asks whether there is an ancestor B' of B and a descendant A' of A, such that the node B' has an edge with destination A' and relation NIL. Why is the predicate "implies-p" defined like this is clear. For `(implies-not-p B A)`, assume that there is an ancestor B-anc of B and a descendant A-desc of A, such that B-anc does not imply A-desc (the meaning of a NIL-edge from B-anc to A-desc). Then `(implies-not-p B A)` must be T, i.e., B does not imply A, because otherwise we have the implication chain:
  B-anc implies B implies A implies A-desc, 
therefore B-anc implies A-desc, contradiction to the NIL-edge from B-desc to A-desc. 

**test.lisp** contains test data and testing functions, which should be run after every and any change in the above files.
Run all tests while `(in-package :jeffrey-test)` with the command `(test-all)`, which prints a report to your REPL.

**/sneak-preview/** contains diagrams with sets of forms that make sense, e.g., between forms about alephs and their properties, as well as diagrams with random sets of forms. A boldfaced arrow from A to B means that the implication is non-reversible, i.e., there exists a model of ZF set theory in which B holds and A doesn't. *Just imagine the endless possibilities for random research projects, theses, and papers, filling or boldfacing those arrows! :)*  The `full-diagram.pdf` is also included, only for standard "number names" (not full statements). Its size is 3,63m x 78 cm. 

**complexity issues**
I haven't really tried to optimize this algorithm yet (from the graph structure to the diagram), apart from the addition of the *jeff-matrix* that remembers the answers of the predicates. This could be made even faster, but it's not necessary now. 

More details to be added.
 
