# Choiceless grapher 

This is the early stage of a program which creates diagrams with the relationships between consequences of the Axiom of Choice. This project is inspired by the encyclopedia of Set Theory without the Axiom of Choice:
                          **Consequences of the Axiom of Choice Project**
by Prof. Paul Howard and Prof. Jean E. Rubin. I thank Paul Howard for providing me with the implication matrix (book1), a tex document with the form statements in LaTeX form, and permision to use these files, which can be found in the folder "Howard-Rubin-data".

A quick overview is given below. A paper with a full description and explanation of the code will appear after I upload the diagram drawing part of this program. A big *thank you* to my Common-Lisp teacher, Max Rottenkolber, for coming up with the graph-structure for this program, for naming Jeffrey, and for his lessons and advice. 

## Description
This program requires package "mpc" and "split-sequence". Quicklisp should be able to quickload these. 

**jeffrey.asd** contains the (defsystem ...) command that creates "jeffrey" as a system of packages. The files that comprise jeffrey's packages and their dependencies are listed in **packages.lisp**.

**graph.lisp** contains the graph structure where the information is stored (the types), and the related functions, which are the basic language of the system. There are two types, one type *node*, which is a list of edges, a list of parents, which should all be of type *node*, and some attributes. The other type is *edge*, which are elements in the list of a node's edges. An edge is a destination (a node), a relation (T or NIL), and some attributes. The nodes are supposed to be stored in a hash-table. This graph structure will soon become obsolete, because the choiceless grapher will use matrices, instead of graph structures, for efficiency. Howard and Rubin's "book1" is such a matrix. 

**read.lisp** contains the functions that read input, which should be implications in the form '(:Form1 :Form0 T), in this example meaning Form1 implies Form0, or '(:Form0 :Form1 NIL), meaning Form0 does not imply Form1. :Form1 is "The axiom of choice." and it is the top node of this lattice. :Form0 is "0=0", intended to mean just ZF, and it is the bottom node. Attributes will be added later. Some constants such as *nodes*, :Form1, and :Form0 are defined here.

**predicates.lisp** enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. (implies-p A B) asks whether A is an ancestor of B and (implies-not-p B A) asks whether there is an ancestor B' of B and a descendant A' of A, such that the *node* B' has an edge with destination A' and relation NIL. Why is the predicate "implies-p" defined like this is clear. Until the paper and the ACL2 proofs appear, a quick drawing should convince you that the definition of the predicate "implies-not-p" is also correct. A few words or an informal proof: Say we want to show that (implies-not-p B A), i.e., that B does not imply A, i.e., that there exists a "model" of ZF-set theory in whicn A is true and B is false. If we establish that there exists an ancestor B' of B and a descendant A' of A, such that the node B' does not imply the node A' (so "(implies-not-p B' A')" is true), then we get that (implies-not-p B A) is true. To see this, assume for a contradiction that (implies-p B A) is true instead, denote this by "B => A" and note that the relation "=>" is transitive. Then we have that "B' => B => A => A'" which means (implies-p B' A') is true, which is a contradiction to (implies-not-p B' A').

**test.lisp** contains test data and testing functions, which should be run after every and any change in the above files.

**read-book1** contains functions that fascilitate parsing and storing (reading in) the Howard-Rubin matrix book1. Book1 has as many row and columns as are forms in the database of their book "Consequences of the Axiom of Choice", plus some newer ones. 

**test-book1** can make comparisons between book1 and a matrix that is induced by the primitive implications (codes 1 and 3), and the predicates implies-p and implies-not-p in :jeffrey.predicates. This is one way to illustrate that both the predicates implies-p and implies-not-p, and the matrix "book1" are created correctly. Some more convincing formal proofs in the common-lisp proof checking language ACL2 concerning these predicates will follow. 

**/sneak-preview/** contains diagrams with sets of forms that make sense, e.g., between forms about alephs and their properties, as well as diagrams with random sets of forms. A boldfaced arrow from A to B means that the implication is non-reversible, i.e., there exists a model of ZF set theory in which B holds and A doesn't. *Just imagine the endless possibilities for random research projects, theses, and papers, filling or boldfacing those arrows! :)*

**complexity issues**
I haven't tried to optimize this algorithm yet (from the graph structure to the diagram). It is okay for small diagrams (for up to 100 forms it takes about 10 minutes on a 12 year old laptop, and this depends on whether the top and bottom nodes are included, and on the chosen forms). Most of the time is spent in evaluating the predicates implies-p and implies-not-p. In the next version of the program, I will use a matrix instead of a graph structure, in which matrix all the "implies-p" and "implies-not-p" questions are answered (book1). I will keep the predicates of course, which can be used to update the matrix as new information comes in. 

More details to be added.
