# Choiceless grapher #

This is an early stage of a program which creates diagrams with the relationships between consequences of the Axiom of Choice. This project is inspired by the encyclopedia of Set Theory without the Axiom of Choice:
                          **Consequences of the Axiom of Choice Project**
by Prof. Paul Howard and Prof. Jean E. Rubin.

So far only the core of the program has been uploaded. A quick overview is given below, a paper with a full description will appear. A big *thank you* to my Common-Lisp teacher, Max Rottenkolber, for coming up with the defstructure for this program, for naming Jeffrey, and for all the lessons. 

**graph.lisp** contains the structures where the information sits (the types), and the related functions, which are the basic language of the system. A graph consists of nodes, with type *node*, which is a list of edges, a list of parents, which should be of type *node*, and some attributes. 
An edge in the list of edges of a node has its own type *edge*, which is a destination (a node), a relation (T or NIL), and some attributes. 

**read.lisp** contains the functions that read input, which should be implications in the form '(:Form1 :Form0 T), meaning Form1 implies Form0, or '(:Form0 :Form1 NIL), meaning Form0 does not imply Form1. :Form1 is "The axiom of choice." and it is the top node of this lattice. :Form0 is "0=0", intended to mean just ZF, and it is the bottom node. Attributes will be added later. Unknown implications are not stored in the graph, these references will be handled separately. Some constants such as *nodes*, :Form1, and :Form0 are defined here.

**predicates.lisp** enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. (implies-p A B) asks whether A is an ancestor of B and (implies-not-p B A) asks whether there is an ancestor B' of B and a descendant A' of A, such that the *node* B' has an edge with destination A' and relation NIL. Until the paper appears, a quick drawing should convince you. 

**test.lisp** contains test data and testing functions, which should be run after every and any change in the above files.

**/sneak-preview/** contains diagrams with sets of forms that make sense, e.g., between forms about alephs and their properties, as well as diagrams with random sets of forms. A boldfaced arrow from A to B means that the implication is non-reversible, i.e., there exists a model of ZF set theory in which B holds and A doesn't. *Just imagine the endless possibilities for random research projects, theses, and papers, filling or boldfacing those arrows! :)*

**complexity issues**
I haven't tried to optimize this algorithm yet. It is okay for small diagrams (for up to 100 forms it takes about 10 minutes on a 12 year old laptop, and this depends on whether the top and bottom nodes are included, and on the chosen forms). 

More details to be added.
