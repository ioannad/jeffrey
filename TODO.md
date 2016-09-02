### TO-DO list for the Choiceless Grapher

# Finish process-strings.lisp

 and make the rest of the modules work with this and the new parse.lisp. 

# Add short names?

This could be very non-straightforward, as there is no obvious way to do this well.

# Consider implementing "levels"

If each node has a "level", where node 1 has level 0 (backwards levels), then if a node has level n, each of its "direct" children have level n+1.

A child B of A is a direct child, when there is no other child of A, which is an ancestor of B.

Preparing the data using levels, would help with the efficiency of the predicates, for example `implies-p` would only have to check between nodes of the correct levels:
If A has level n, B has level m, and n<m, then (implies-p A B) is automatically false (in fact, `ancestor`).

It could similarly improve `descendant`. But is it worth it? 
