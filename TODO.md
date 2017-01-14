# TO-DO list for the Choiceless Grapher

Active to-dos are on top.

### Create proper website and set up Choiceless Grapher to be used via a web form.

cgraph.inters.co is up and running!

Still to do:

* Add intervals feature
* Remove features that produce huge graphs: descendants/ancestors, and "Show negative implications" only for graphs with under 10 nodes.

* Shorthand names (longer term), which can be used in a possible:

* matrix or list of forms (click-and-add-to-graph sort of feature).

### Add short names?

This seems very complicated (manual work). See above.

### Create fancy labels on the fly

Instead of using the labelmaker in bulk.

### Improve predicate algorithms

Reduced complexity of `graph-implies-not-p` by switching from a nested `(some ... (some ...))` to two separate loops. Measured improvement of about 7% on SBCL and 25% on CCL (notes available).

### Figure out pdf output issue

PDF output does not render fancy labels properly. Why?
For the moment I use png, which is the current default. 

### Add Windows and Macintosh support.

The external-programs only work for Linux for the moment. I consider the website as a "fix enough for now" for this issue, but a desktop version should appear in the future.


