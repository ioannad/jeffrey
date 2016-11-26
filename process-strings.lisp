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

(assert (equal "bbcdefg" (search-replace "a" "b" "abcdefg")))

(defvar *comments* ""
  "To collect any comments in {FORMSNUM.TEX}, as this may involve 
information about forms being equivalent.")

(defun extract-comments (string)
  "Removes a comment from `string` and concatenates it to
`*comments*`. Returns "
  (let ((comment-beginning (search "%" string)))
    (if comment-beginning
	(let ((new-string (subseq string
				  0
				  comment-beginning))
	      (comment (subseq string
			       (+ 1 comment-beginning)
			       (length string))))
	  (setf *comments* (format nil "~a~%~%~a" *comments* comment))
	  new-string)
	string)))

(defun test-extract-comments ()
  (setf *comments* " ")
  (assert (and (equal "abc " (extract-comments "abc %qwe"))
	       (equal *comments* " 

qwe"))))

(defun this-with-pairs ()
  "The substitutions will be performed backwards in 
process-string."
  (reverse '(("\\item\\item{}" "\\itemitem")
	     ("\\item{}"       " ")
	     ("\\leqno(*)"     "($*$)")
	     ("\\item {"       "\\itemitem{")
	     ("$$"             "$")
	     ("\\tag"          " "))))

(defun process-string (string this-with-pairs%)
  (if (null string)
      ""
      (if this-with-pairs%
	  (let ((this (first  #1=(first this-with-pairs%)))
		(with (second #1#))
		(rest-pairs (rest  this-with-pairs%)))
	    (search-replace this
			    with
			    (process-string string rest-pairs)))
	  string)))

(assert (equal (process-string "There $$5$$ are no $\\aleph_{\\alpha}$ minimal  sets.  \\item{}That is, \\leqno(*) there are no sets\\item { $X$} such that \\item\\item{}{(1)} $|X|$ is incomparable with $\\aleph_{\\alpha}$"
			       (this-with-pairs))
	       "There $5$ are no $\\aleph_{\\alpha}$ minimal  sets.   That is, ($*$) there are no sets\\itemitem{ $X$} such that \\itemitem{(1)} $|X|$ is incomparable with $\\aleph_{\\alpha}$"))

(defvar *badends*
  '("\\iput{" "\\rightheadtext{" "\\medskip  \\end-"))

  
(defun drop-badends (string)
  "Removes the tail of `string` which begins with a string 
in {*badends*}."
  (let ((positions (mapcar (lambda (badend)
			     (search badend string))
			   *badends*)))
    (if #1=(remove-if #'null positions)
	(let ((pos (first (sort #1# #'<))))
	  (subseq string 0 pos))
	string)))
	   
(assert (equal "a" (drop-badends "a\\iput{asd"))) 
(assert (equal "b"
	       (drop-badends
		"b\\rightheadtext{w2d \\iput{sg")))

(defun process-subform (id name LaTeX references)
  (flet ((process (string) (drop-badends
			    (extract-comments
			     (process-string string
					     (this-with-pairs))))))
    (list id
	  (concatenate 'string "{HR " name "} " (process LaTeX))
	  (process references))))
		   
(assert (equal '(2 "{HR name} latex" "ar")
	       (process-subform 2 "name"
				"latex\\rightheadtext{njf"
				"ar\\iput{kjg")))

(defun process-main-form (form-name LaTeX references) 
  (let ((form-number (parse form-name (=natural-number))))
    (process-subform form-number form-name LaTeX references)))

(defun =eq-form-name ()
  (=list (?char #\[)
	 (=natural-number)
	 (?whitespace)
	 (=subseq (%some (?satisfies 'upper-case-p)))))

(assert (equal '(NIL 0 NIL "AD")
	       (parse "[0 AD($p$)" (=eq-form-name))))

(defun process-eq-form (eq-form-name LaTeX references form-number)
  (let ((eq-form-number (second #1=(parse eq-form-name
					 (=eq-form-name))))
	(eq-form-id (fourth #1#)))
    (assert eq-form-id)
    (assert (equal form-number eq-form-number))
    (process-subform eq-form-id eq-form-name LaTeX references)))

(assert (equal
	 '(430 "{HR 430($p$)}  A $ " "\\ac{B}")
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
	 (process-forms '((("0" "a" "b") ("[0 C]"  "d" NIL))
			  (("1" "e" NIL) ("[1 FG]" "h" "i"))))
	 '(((0 "{HR 0} a" "b") ("C" "{HR [0 C]} d" ""))
	   ((1 "{HR 1} e" "") ("FG" "{HR [1 FG]} h" "i")))))
