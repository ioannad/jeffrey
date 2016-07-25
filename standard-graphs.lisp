#|
Some standard graphs.
|#

(defun create-standard-graphs ()
  
  (graph-descendants
   '(8)
   "consequences-of-HR8-C-omega-infty"
   "fancy"
   "png")

  (graph-descendants
   '(85)
   "consequences-of-HR85-C-infty-omega"
   "fancy"
   "png") 
  
  (graph-descendants
   '(8 85)
   "consequences-of-HR8-and-HR85-cntbl-choice-forms"
   "fancy"
   "png") 

 )
