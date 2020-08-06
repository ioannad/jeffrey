# Choiceless grapher

The Choiceless Grapher can produce any size of graph of the implication relationships between the consequences of the axiom of choice, [as found here](http://consequences.emich.edu/conseq.htm), with an option on the style of nodes: you can either have the Howard-Rubin (HR) numbering of the forms ("numbers"), or the full LaTeX-formatted statements ("fancy"). It's online as an [app here](http://cgraph.inters.co).

This project is inspired by and based on the **Consequences of the Axiom of Choice Project**, the encyclopedia of set theory without the axiom of choice, by *Prof. Paul Howard and Prof. Jean E. Rubin*. I thank Paul Howard for providing me with the original implication matrix (book1), a tex document with the form statements in LaTeX form, and permision to use these files, which can be found in the folder "Howard-Rubin-data".

An overview of the program is given below. A detailed description was published in my previous website, which [no longer exists](https://boolesrings.org/ioanna/). I plan to import the old posts from there to my new website [ioa.re](https://ioa.re).

This is my first fullstack program, and I pledge to maintain it for life. I wrote this while learning common-lisp, and for this, a big **thank you** to my common-lisp sensei [Max Rottenkolber](http://mr.gy) over at [interstellar ventures](http://inters.co) for showing me the light (common-lisp), for naming back then this choiceless grapher `jeffrey` (because naming is hard), and for hosting this program's web app:

## The Website, aka the CGraph app

The easiest way to use this program is to use its online app here: [cgraph.inters.co](http://cgraph.inters.co). Just enter the HR. numbers of the axioms you want to draw, possibly change the options and hit "Request diagram".

The website has only minimal information, but you can [read more here](https://boolesrings.org/ioanna/2016/12/13/choiceless-grapher-app/).

If you want very large diagrams (more than 70 or 80 forms)*, or if you prefer to work in a CL REPL, do use the program as described below.

(*) The web app can indeed create very large diagrams (even the full diagram) but it takes some time, and if you need more than one, it's probably faster to use jeffrey in a CL REPL.

## DISCLAIMER

**THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND WHATSOEVER.**

## The program

### Requirements

* Common-Lisp: I have only tested this with **SBCL** or **CCL**  but it should work in any implementation that [external-program](https://github.com/sellout/external-program) supports. Please let me know if you check this!
* [Graphviz](www.graphviz.org/) (`apt-get install graphviz`)
* [Quicklisp](www.quicklisp.org)
* It only works in Linux. Windows and Macintosh support are no longer on the to-do list, since the program is accessible via its web-interface. *If you are interested in having these OS supported please send me a message.* Having said that, do try it in a Mac, as any unix system which understands `which` should work in theory. Please let me know!
* The package `labelmaker` (so to use "fancy" labels) also requires `/bin/bash`, `pdflatex`, the latex package `standalone`, and `imagemagick` for `convert`. Currently there are some issues with `convert`ing from `pdf` files, please wait for the next update to use fancy labels.

### Installation

Install this package using quicklisp ([installation instructions](https://www.quicklisp.org/beta/#installation)) and git ([installation instructions](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)) as follows:

- Install the system dependency Graphviz, for example with aptitude (Debian, Ubuntu, et.al.) using `sudo apt install graphviz`.
- In a terminal, navigate to a directory that ASDF or quicklisp can find (e.g. `quicklisp/local-projects/`) and `git clone https://gitlab.common-lisp.net/idimitriou/jeffrey.git`. Alternatively otherwise download the contents of this repository to such a directory.

### Load the Choiceless Grapher

In a Common Lisp REPL, evaluate `(ql:quickload "jeffrey")` and `(in-package :jeffrey)`.

### Draw the diagram between a given list of form HR numbers

If `'(a b c ..)` is the list of form numbers (only numbers, without parameters or equivalent form letters), whose relationships you wish to graph, e.g. `'(1 2 3 4 5)` and if you want to save the diagram in a `png` file `filename.png`, then evaluate:

`(graph '(1 2 3 4 5) "filename.png")`

Instead of `png` you can create another output format supported by `dot`, e.g., `svg`, `pdf`, `jpg`, etc. See `man dot` for details.
If you leave out the `.*` ending, a `png` file will be produced.

All output files are created in a (new) `diagrams/` subdirectory of this repository's base directory.

### Drawing a random diagram of a given size

You can also draw the implication diagram of a pseudo-random collection of forms, of a given size, as follows (for size 6):

`(random-graph 6 "filename")`

"Random" diagrams additionally include forms 1 (Axiom of Choice, top form) and form 0 (`0=0`, bottom form), otherwise these diagrams are often completely disconnected.

### Drawing descendants of a list of forms

To draw the consequences of forms in a list (e.g. all consequecnes of form 8 and of form 85), evaluate:

`(graph-descendants '(8 85) "filename")`

### Drawing ancestors of a list of forms

To draw all the forms that imply a form in a list of forms evaluate:

`(graph-ancestors '(399 256) "filename")`

### *NEW:* Drawing an interval between two forms

To draw all the consequences of the form with `name-1` (e.g., 261) that imply the form with `name-2` (e.g., 260), evaluate:

`(graph-interval 261 260 "filename")`

## Detailed description

This program requires the common-lisp packages "maxpc", "split-sequence", and "external-program". They are all available via Quicklisp, so you shouldn't have to install these separately. For the website it depends additionally in the quicklisp packages "

It's nickname is `jeffrey`, as naming is hard and this was my first full stack program.

**jeffrey.asd** contains the (defsystem ...) command that creates "jeffrey" as a system of packages. The files that comprise jeffrey's packages, their exported functions, and their dependencies are listed in **packages.lisp**.

**graph.lisp** contains the graph structure where the information is stored (the types), and the related functions, which are the basic language of the system. There are two types, one type `node`, which is a name of type natural number, a list of edges of type edge described below, a list of parents of type node, a LaTeX statement of type string, references of type string, and a placeholder for attributes. The other type is `edge`, which is a destination of type node, a relation (T or NIL), which corresponds to positive and negative implication arrow respectively, and a placeholder for attributes. The nodes are to be stored in the exported hash-table `*graph*`.

**parse.lisp** contains parsing functions for reading in node (form) information, and for reading `book1`, the original matrix with all the implication codes. Form information, i.e., name, LaTeX-statement, and references are parsed from the TeX-file `Howard-Rubin-data/FORMSNUM.TEX`. Implication information is parsed simply, because book1 is a simple integer matrix whose lines terminate with -1. I use Max's Parser Combinators ([maxpc](https://github.com/eugeneia/maxpc)).

**process-strings.lisp** contains functions which make the text in any LaTeX-statement LaTeX compatible (the origin is TeX). It's a crude search and replace routine.

**read.lisp** contains the functions that read input, and it can be run in its whole with `(read-all-data)`. This function will first store the form data from `FORMSNUM.TEX` as nodes in `*graph*`, then add edges and parents to these nodes, following only the direct i nformation from `book1`. That is, if book1 has code 1 in position (i,j), then it will add an edge to the node with name i (node i) with destination node j and relation T, and it will add node i to the set of parents of node j. If book1 has code 3 in position (i,j), then it will only add an edge to node i with destination node j and relation NIL. All other codes should be derivable from this information, using the predicates in the next module.

**predicates.lisp** enables the program to ask whether or not a node (form) implies another. The function implies-p only answers positive implication questions, and implies-not-p only answers negative implication questions. In particular, `(implies-p A B)` asks whether `A` is an ancestor of `B` and `(implies-not-p B A)` asks whether there is an ancestor `B-anc` of `B` and a descendant `A-desc` of `A`, such that the node `B-anc` has an edge with destination `A-desc` and relation NIL (i.e., such that `B-anc` does not imply `A-desc`). Why is the predicate `implies-p` defined like this is clear. For `(implies-not-p B A)`, assume that there is an ancestor `B-anc` of `B` and a descendant `A-desc` of `A`, such that `B-anc` does not imply `A-desc` (the meaning of a NIL-edge from `B-anc` to `A-desc`). Then `(implies-not-p B A)` must be T, i.e., it must be the case that `B` does not imply `A`, *otherwise* we have the implication chain:

  `B-anc` implies `B` implies `A` implies `A-desc`,

therefore `B-anc` implies `A-desc`, which is a contradiction to the NIL-edge from `B-anc` to `A-desc`.

**test.lisp** contains test data and testing functions, which should be run after every and any change in the above files.
Run all tests while `(in-package :jeffrey-test)` with the command `(test-all)`, which prints a report to your REPL.

**draw.lisp** draws diagrams with the command `(draw '(a b c d ...) "filename" "style")` where `'(a b c d ...)` is a list of natural numbers up to 430, excluding 360, and 423 and 374 for the moment. Requires the database, i.e., `*graph*` to be loaded and `*jeff-matrix*` initiated. Normal users please use the `:jeffrey.main` package.")

**labelmaker.lisp** creates the fancy labels that dot may use. I hope to make this obsolete at some point, and create the labels on the fly.

**main.lisp** the main package is explained above.

**website.lisp** "The website of choiceless grapher, uses [hunchentoot](https://www.cliki.net/Hunchentoot) as a webserver, and [html-template](https://www.cliki.net/html-template) to create the pages. Thanks to [Edi Weitz](https://www.cliki.net/Edi%20Weitz) for these two brilliant packages!

**diagrams/** is not included here, it is created when you create your first diagram. All diagrams you create with this program are put there.

**examples/** contains diagrams with sets of forms that make sense, e.g., between forms about alephs and their properties, as well as diagrams with random sets of forms. A boldfaced arrow from A to B means that the implication is non-reversible, i.e., there exists a model of ZF set theory in which B holds and A doesn't. *Just imagine the endless possibilities for random research projects, theses, and papers, filling or boldfacing those arrows! :)*  The `full-diagram.pdf` is also included, only for standard "number names" (not full statements). Its size is 3,41m x 1,72 m.

**fancy-labels/** is not included here, but you can produce it from package `:jeffrey.labelmaker` with the command `(make-fancy-labels`). Please wait for the next update to use this.

**Howard-Rubin-data/** contains the files from the Consequences of the Axiom of Choice Project, which were kindly provided by Prof. Paul Howard. 

**complexity issues**
I have made some small steps to improve the original brute force algorithm for calculating the predicates. The biggest difference was made by memoising with the addition of the `*jeff-matrix*`. A 7%-25% improvement (SBCL and CCL respectively, at the time of testing, on Debian in my thinkpad x201) was achieved by changing the double loop of `(graph-implies-not-p Y X)` to three single loops (finding destinations of nil edges of ancestors of Y, finding descendants of X, and intersecting these two lists).
