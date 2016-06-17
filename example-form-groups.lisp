(in-package :jeffrey.main)

#| 
This file contains some groups of form HR. numbers, whose diagrams 
I'd like to generate as examples. 
|#

(defvar *old-collection* 
  '(0 1 13 22 34 35 36 37 38 51 93 170 203 212 273 363 368 369))

(defvar *alephs-and-their-properties*
  '(25 34 40 41 54 56 58 71 104 108 159 170 182 183 208 209 245 246
    275 315 365))

(defvar *all-forms*
  (loop for name being the hash-keys of *graph*
     collect name))
