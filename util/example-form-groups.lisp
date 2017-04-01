(in-package :jeffrey.main)

#| 
This file contains some groups of form HR. numbers, whose diagrams 
I'd like to generate as examples. 
|#

(defun examples ()
  (list
   '("old-collection"
     (0 1 13 22 34 35 36 37 38 51 93 170 203 212 273 363 368 369))

   '("alephs-and-their-properties"
     (25 34 40 41 54 56 58 71 104 108 159 170 182 183 208 209 245
       246 275 315 365))
   
   '("HR-0-10" (0 1 2 3 4 5 6 7 8 9 10))

   '("HR-1-10" (1 2 3 4 5 6 7 8 9 10))

   '("HR-2-10" (2 3 4 5 6 7 8 9 10))

   (list "HR-0-22" (loop for i from 0 to 22 collect i))))

(defun make-examples (&optional (style "fancy"))
  (loop for (filename names-list) in (examples)
     do (graph names-list filename style))
  (graph-descendants '(132) "descendants-of-132" style)
  (random-graph 10 "random-10" style)
  (graph (loop for name being the hash-keys of *graph*
	    collect name)
	 "full-diagram" "numbers" "png"))
