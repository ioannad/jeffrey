(in-package :jeffrey.process-strings)

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

(defvar *this-with-pairs*
  "The substitutions will be performed backwards in 
process-string."
  (reverse '('("\\item\\item{}" "\\itemitem")
	     '("\\item{}"       " ")
	     '("\\leqno(*)"     "($*$)")
	     '("\\item {"       "\\itemitem{")
	     '("$$"             "\\\\"))))

(defun process-string (string this-with-pairs)
  (let ((first-pair (first this-with-pairs))
	(rest-pairs (rest  this-with-pairs)))
    (destructuring-bind (this with) first-pair
      (search-replace this
		      with
		      (process-string string rest-pairs)))))

(defun process-form (main-form)
  (=destructuring-bind (form-name LaTeX references)
		       (list (parse-integer form-name :junk-allowed t)
			     (concatenate 'string
					  "{HR " form-name ".} "
					  (process-string LaTeX))
			     (process-string references))))

;; Check if eq-forms have the same number as their main form
