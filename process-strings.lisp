(In-package :jeffrey.process-strings)

#| 
This is an inefficient way to parse strings. Since the target 
strings are quite small, and I want to experiment with strings,
I add this anyway instead of using a library such as CL-PPCRE.

This package is specifically targeted for the strings in the 
full-name an references of the subforms (main-form or its 
equivalents).

It should only be references from read.lisp.
|#

(defun search-replace (this with in-string)
  (let ((start-pos  (search this in-string)))
    (if start-pos
	(let* ((end-pos    (+ start-pos (length this)))
	       (left-part  (subseq in-string
				   0
				   start-pos))
	       (right-part (subseq in-string
				   end-pos
				   (length in-string))))
	  (concatenate 'string
		       left-part
		       with
		       (search-replace this with right-part)))
	in-string)))

(defvar *comments* '()
  "To collect any comments in {FORMSNUM.TEX}, as this may involve 
information about forms being equivalent.")

(defun extract-comments (string)
  "Removes a comment from `string` and pushes it into 
`*comments*`."
  (let ((comment-beginning (search "%" string)))
    (if comment-beginning
	(let ((new-string (subseq string
				  0
				  comment-beginning))
	      (comment (subseq string
			       comment-beginning
			       (length string))))
	  (push comment *comments*)
	  new-string)
	string)))

(defun this-with-pairs ()
  "The substitutions will be performed backwards in 
process-string."
  (reverse '(("\\item\\item{}" "\\itemitem")
	     ("\\item{}"       " ")
	     ("\\leqno(*)"     "($*$)")
	     ("\\item {"       "\\itemitem{")
	     ("$$"             "\\\\"))))

(defun process-string (string this-with-pairs%)
  (if this-with-pairs%
      (let ((this (first  #1=(first this-with-pairs%)))
	    (with (second #1#))
	    (rest-pairs (rest  this-with-pairs%)))
	(search-replace this
			with
			(process-string string rest-pairs)))
      string))

(assert (equal (process-string "There $$ are no $\\aleph_{\\alpha}$ minimal  sets.  \\item{}That is, \\leqno(*) there are no sets\\item { $X$} such that \\item\\item{}{(1)} $|X|$ is incomparable with $\\aleph_{\\alpha}$"
			       (this-with-pairs))
	       "There \\\\ are no $\\aleph_{\\alpha}$ minimal  sets.   That is, ($*$) there are no sets\\itemitem{ $X$} such that \\itemitem{(1)} $|X|$ is incomparable with $\\aleph_{\\alpha}$"))

(defun process-subform (id name LaTeX references)
  (list id
	(concatenate 'string "{HR " name ".} "
		     (extract-comments
		      (process-string LaTeX (this-with-pairs))))
	(extract-comments
	 (process-string references (this-with-pairs)))))

(defun process-main-form (form-name LaTeX references) 
  (let ((form-number (parse form-name (=natural-number))))
    (process-subform form-number form-name LaTeX references)))

(defun =eq-form-name ()
  (=list (=natural-number)
	 (?whitespace)
	 (=subseq (%some (?satisfies 'upper-case-p)))))

(assert (equal '(0 NIL "AD")
	       (parse "0 AD($p$)" (=eq-form-name))))

(defun process-eq-form (eq-form-name LaTeX references form-number)
  (let ((eq-form-number (first #1=(parse eq-form-name
					 (=eq-form-name))))
	(eq-form-id (third #1#)))
    (assert eq-form-id)
    (assert (equal form-number eq-form-number))
    (process-subform eq-form-id eq-form-name LaTeX references)))

(assert (equal
	 '(430 "{HR 430($p$).}  A \\\\ " "\\ac{B}")
	 (process-main-form "430($p$)"  " A $$ " "\\ac{B}")))

(defun process-form (form)
  (let* ((main-form   (apply #'process-main-form (first form)))
	 (form-number (first main-form)))
    (append
     (list main-form)
     (loop for (eq-form-name LaTeX references) in (rest form)
	collect (process-eq-form eq-form-name
				 LaTeX
				 references
				 form-number)))))
	  
(defun process-forms (forms)
  (mapcar #'process-form forms))
	     
(assert (equal 
	 (process-forms '((("0" "a" "b") ("0 C"  "d" NIL))
			  (("1" "e" NIL) ("1 FG" "h" "i"))))
	 '(((0 "{HR 0.} a" "b") ("C" "{HR 0 C.} d" NIL))
	   ((1 "{HR 1.} e" NIL) ("FG" "{HR 1 FG.} h" "i")))))
	 
	   

;; Check if eq-forms have the same number as their main form
