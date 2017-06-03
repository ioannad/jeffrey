# TO-DO list for the Choiceless Grapher

Active to-dos are on top.

## Currently in my scope

* Create build process - perhaps even resulting in an executable? (Though I personally prefer to use this in a REPL).

* Add intervals feature

* "Show negative implications" (only for graphs with under 10 nodes.)

* Generalise jeffrey to any database (with area for own databases on the web interface)

## More to-do ideas with comments

### Add short names?

This seems very complicated (i.e., manual work).

### Create fancy labels on the fly

Instead of using the labelmaker in bulk. *Not sure if this is a good idea.*

### Further improve predicate algorithms

Reduced complexity of `graph-implies-not-p` by switching from a nested `(some ... (some ...))` to two separate loops. Measured improvement of about 7% on SBCL and 25% on CCL (notes available).

### Figure out pdf output issue

PDF output does not render fancy labels properly. Why?
For the moment I use png, which is the current default. 

### ~~Add Windows and Macintosh support.~~

The external-programs (bash scripts, calls to the CL-library `external-program`) only work with Linux. I am withdrawing claims to future MS/iOS support, as there are no interested users, as far as I know. The occassional MS/iOS users seem happy with the web interface. If you are an MS/iOS user and want to use this program in a CL REPL please let me know.


