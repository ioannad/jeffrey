# Choiceless grapher 

This is the early stage of a program which creates diagrams with the relationships between consequences of the Axiom of Choice. This project is inspired by the encyclopedia of Set Theory without the Axiom of Choice:
                          **Consequences of the Axiom of Choice Project**
by Prof. Paul Howard and Prof. Jean E. Rubin. I thank Paul Howard for providing me with the implication matrix (book1), a tex document with the form statements in LaTeX form, and permision to use these files, which can be found in the folder "Howard-Rubin-data".

A quick overview is given below. A paper with a full description and explanation of the code will appear after I upload the diagram drawing part of this program. A big *thank you* to my Common-Lisp teacher, Max Rottenkolber, for coming up with the graph-structure for this program, for naming Jeffrey, and for his lessons and advice. 

## Description
This program requires package "mpc" and "split-sequence". 

**jeffrey.asd** contains the (defsystem ...) command that creates "jeffrey" as a system of packages. The files that comprise jeffrey's packages and their dependencies are listed in **packages.lisp**.

**graph.lisp** contains the graph structure where the information is stored (the types), and the related functions, which are the basic language of the system. There are two types, one type *node*, which is a list of edges, a list of parents, which should all be of type *node*, and some attributes. The other type is *edge*, which are elements in the list of a node's edges. An edge is a destination (a node), a relation (T or NIL), and some attributes. The nodes are stored in the hash-table *nodes* of package :jeffrey.read.

**read.lisp** contains the functions that read input, which should be implications in the form '(:Form1 :Form0 T), in this example meaning Form1 implies Form0, or '(:Form0 :Form1 NIL), meaning Form0 does not imply Form1. :Form1 is "The axiom of choice." and it is the top node of this lattice. :Form0 is "0=0", intended to mean just ZF, and it is the bottom node. Attributes will be added later. Some constants such as *nodes*, :Form1, and :Form0 are defined here. MPC-parsing functions for book1 are here, as well as some basic functions for the predicate matrix of package :jeffrey.predicates.

**predicates.lisp** enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. (implies-p A B) asks whether A is an ancestor of B and (implies-not-p B A) asks whether there is an ancestor B' of B and a descendant A' of A, such that the *node* B' has an edge with destination A' and relation NIL. Why is the predicate "implies-p" defined like this is clear. Until the paper and the ACL2 proofs appear, a quick drawing should convince you that the definition of the predicate "implies-not-p" is also correct. 

*The idea behind the predicates:* Say we want to show that (implies-not-p B A), i.e., that B does not imply A, i.e., that there exists a "model" of ZF-set theory in whicn A is true and B is false. If we establish that there exists an ancestor B' of B and a descendant A' of A, such that the node B' does not imply the node A' (so "(implies-not-p B' A')" is true), then we get that (implies-not-p B A) is true. To see this, assume for a contradiction that (implies-p B A) is true instead, denote this by "B => A" and note that the relation "=>" is transitive. Then we have that "B' => B => A => A'" which means (implies-p B' A') is true, which is a contradiction to (implies-not-p B' A'). These predicates work with the help of universal variable *predicate-matrix* and reasoning functions on the graph structure.

**test.lisp** contains test data and testing functions, which should be run after every and any change in the above files.

**/sneak-preview/** contains diagrams with sets of forms that make sense, e.g., between forms about alephs and their properties, as well as diagrams with random sets of forms. A boldfaced arrow from A to B means that the implication is non-reversible, i.e., there exists a model of ZF set theory in which B holds and A doesn't. *Just imagine the endless possibilities for random research projects, theses, and papers, filling or boldfacing those arrows! :)*  The full-diagram.pdf is also included, only for standard "number names" (not full statements). Its size is 3,63m x 78 cm. 

**complexity issues**
I haven't really tried to optimize this algorithm yet (from the graph structure to the diagram), apart from the addition of the *predicate-matrix* that remembers the previous answers. This could be made even faster, but it's not necessary now. 

More details to be added.
