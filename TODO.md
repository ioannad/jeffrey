# TO-DO list for the Choiceless Grapher

Active to-dos are on top.

### Create proper website and set up Choiceless Grapher to be used via a web form.

**website.lisp created** and works in returning correct diagrams which resize to the browser window.

Still to do:

* The form still accepts junk, satinize input and handle errors

* Add more user input options: "Add top and bottom nodes", "Random diagram" (takes No of nodes as input), "Descendants of given forms"

* Add some classic diagrams and examples: full diagram, old collection, 0-22, 1-10, 2-22, alephs and their properties


### Create fancy labels on the fly

Instead of using the labelmaker in bulk.

### Improve predicate algorithms

Reduced complexity of `graph-implies-not-p` by switching from a nested `(some ... (some ...))` to two separate loops. Measured improvement of about 7% on SBCL and 25% on CCL (notes available).

### Figure out pdf output issue

PDF output does not render fancy labels properly. Why?
For the moment I use png, which is the current default. 

### Add short names?

This seems very complicated.

### Add Windows and Macintosh support.

The external-programs only work for Linux for the moment.


