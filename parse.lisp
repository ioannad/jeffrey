(in-package :jeffrey.parse)

#|
Parse form information, returning lists of lists, whose first items
are the main forms and their statements. The rest are their 
equivalent forms.
|#

(defun =lexer ()
  "Cuts up text in words."
  (=zero-or-more (=skip-whitespace
		  (=string-of (=not (=whitespace))))))


(defun read-formsnum (file) ;;=> list of strings
  "Reads the file FORMSNUM.TEX and returns a list of its words."
  (let ((text nil))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil 'eof)
	 until (eq line 'eof)
	 do (setf text (append text (run (=lexer) line)))))
    text))

(defun separate-forms-rough (list-of-strings)
  "Takes the list of words created by {read-formsnum} and returns 
a list of lists of words, each these lists containing one form and 
its equivalent statements."
  (split-sequence "\\noindent{\\bf" list-of-strings :test #'equal))

(defun separate-equivalent-forms (rough-forms)
  "Takes the list of lists containing the forms created by 
{separate-forms-rough} and returns a list of lists of lists of 
words. The first, and often the unique, element of each list of 
lists of words contains only the main form number, the LaTeX-
formatted statement, and perhaps some references. "
  (let ((forms-list nil))
    (loop for item in rough-forms
       when (equal (first item)
		   "FORM")
       do (push
	   (split-sequence "\\item{}{\\bf"
			   item :test #'equal) forms-list))
    forms-list))


(defun =reference-beginning ()
  "So far I only saw references in {FORMSNUM.TEX} that start with
the strings: \"(See\", \"van \\ac\", \"\\ac\", \"Note\", \"Clear\", and 
\"G\\.\". This parses exactly these."
  (=or (=string "(See")
       (=string "van \\ac")
       (=string "\\ac")
       (=string "Note")
       (=string "Clear")
       (=string "G\\.")))

(defun form-delimiter-p (string)
  "Returns T if the string is either \\medskip or \\smallskip."
  (or (equal string "\\medskip")
      (equal string "\\smallskip")))

(defun =badstring ()
  "Parses strings that are considered 'bad', in the sense that a 
list of words has to be trimmed, if it contains strings that start
with the strings parsed here. So far I found: 
\"\\iput{\" and \"\\rightheadtext{\"."
  (=or (=string "\\iput{")
       (=string "\\rightheadtext{")))

(defun drop-badstrings (list-of-strings)
  "Removes the tail of a list that contains an element which begins
with a string parsed by {=badstring}."
  (let ((n (length (member-if (lambda (string)
				(run (=badstring)
				     string))
			      list-of-strings))))
    (butlast list-of-strings n)))

(assert (equal '("a" "b") (drop-badstrings
			   '("a" "b" "\\iput{asd" "c")))) 
(assert (equal '("a" "b") (drop-badstrings
			   '("a" "b" "\\rightheadtext{w2d" "c"))))

(defun zip-strings-if (predicate list-of-words)
  "Takes a predicate on strings and a list of words, and returns 
the results of concatenating all the words in {list-of-words} 
which satisfy {predicate}, with spaces in between."
  (let ((result ""))
    (loop for word in list-of-words
       when (funcall predicate word)
       do (setf result (concatenate 'string
				    result
				    word
				    " ")))
    result))

;; For the moment we ignore the equivalent forms. 
(defun get-main-form (main-form) ;;=> '(form-number LaTeX-statement references)
  (let* ((references (member-if (lambda (string)
				  (run (=reference-beginning) string))
				main-form))
	 (n          (length references))
	 (LaTeX      (cddr (butlast main-form n))))
    (list (string-trim ".}" (second main-form))
	  (zip-strings-if (lambda (string) (eql string string))
			  (drop-badstrings LaTeX))
	  (zip-strings-if (lambda (string) (not (form-delimiter-p string)))
			  (drop-badstrings references)))))

    ;; the first elements of the lists representing all forms
    ;; in formsnum contain the main form number and statement.

(defun read-forms-rough (file)
  "Reads in the `file` (should be //Howard-Rubin-data//FORMSNUM.TEX) 
and returns a list with the main form data."
  (let ((formsnum (read-formsnum file)))
    (loop for main-form in (map 'list #'first
				(separate-equivalent-forms
				 (separate-forms-rough formsnum)))
       collect (get-main-form main-form))))

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
  (let ((comment-beginning (search "%" string)))
    (if comment-beginning
	(let ((new-string (subseq string
				  0
				  comment-beginning))
	      (comment (subseq string
			       comment-beginning
			       (length string))))
	  (progn (push comment *comments*)
		 new-string))
	string)))

       

(defun process-string (string)
  "Ugliest function ever written?"
  (let* ((process1 (search-replace "\\item\\item{}"
				   "\\itemitem"
				   string))
	 (process2 (search-replace "\\item{}"
				   " "
				   process1))
	 (process3 (search-replace "\\leqno(*)"
				   "($*$)"
				   process2))
	 (process4 (extract-comments process3))
	 (process5 (search-replace "\\item {"
				   "\\itemitem{"
				   process4))
	 (process6 (search-replace "$$"
				   "\\\\"
				   process5)))
    process6))

(defun process-forms (forms)
  "Some corrections to make the strings LaTeX-compatible."
  (loop for (form-name LaTeX references) in forms
     collect (list (parse-integer form-name :junk-allowed t)
		   (concatenate 'string "{HR " form-name ".} " (process-string LaTeX))
		   (process-string references))))

(defun collect-forms (formsnum.tex)
  (process-forms (read-forms-rough formsnum.tex)))
		 
;; ------------------------------------------------------------------
;; Print the forms in a LaTeX file, to check that they were parsed
;; correctly:
;; ------------------------------------------------------------------

(defun print-forms-file-heading (stream)
  (format stream
	  "\\documentclass[11pt,a4 paper]{article}
   \\usepackage{amssymb,amsmath,longtable,tabu}
   \\newcommand{\\ac}{} 
   \\newcommand{\\icopy}{\\emph}
   \\renewcommand{\\cite}[1]{({#1})}
   \\newcommand{\\Cal}{\\mathcal}
   \\newcommand{\\itemitem}[1]{\\\\ ~%\\hspace{0.2cm}\\mbox{#1}.\\vspace{0.2cm}}       
   \\title{Forms and their statements, from Paul Howard and Jane Rubin's\\\\ ``Consequences of the Axiom of Choice'' project.}
   \\begin{document}
   \\maketitle   
   This is a list of the consequences of the axiom of choice, which contains ``Howard-Rubin'' numbers (HR), their statements, and references to the locations in which they were found by Paul Howard and Jane Rubin. The references and more information can be found in the book {\\bf Consequences of the Axiom of Choice}. All information has been extracted from the file ``FORMSNUM.TEX'' which was kindly provided by Paul Howard.\\~%
\\begin{itemize}~%"))

(defun print-form-item (form stream)
  (format stream "~{ \\item~a ~%{\\bf References:} ~a~}\\\\~%" form))

(defun print-forms-file-ending (stream)
  (format stream "\\end{itemize}
                    ~%Extracted comments: 
                    ~%
                    ~% ~a
                    ~%
                    ~%\\end{document}" *comments*))

(defun print-forms-to-file (forms filename)
  "Creates or overwrites {filename}.tex with a LaTeX document with 
the form statements and references, to make sure that the forms 
have been parsed correctly."
  (let ((file (concatenate 'string filename ".tex")))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede)
      (print-forms-file-heading stream)   
      (loop for form in forms
	 do (print-form-item form stream))
      (print-forms-file-ending stream))))



;;; The functions defined here convert the matrix that is stored in
;;; book1 of Howard and Rubin's "Consequences of the Axiom of
;;; Choice" into a graph structure. 
;;; Parsing is done with Max Rottenkolber's Monadic Parser
;;; Combinator (mpc) library. 


;;; Parsing is much easier for book1:

(defun =field ()
  "A field is a code number for the implication 'Form #row' => 
'Form-#column', where #row or #column is the number of the row or 
column respectively."
  (=skip-whitespace (=natural-number)))

(defun =row-delim ()
  "Book1 ends rows with a -1"
  (=skip-whitespace (=string "-1")))

(defun =row ()
  "A row is a list of one or more fields that ends in -1."
  (=prog1 (=one-or-more (=field)) (=row-delim)))

(assert (equal '(1 2 3) (run (=row) "1 2 3 -1")))

(defun =book1 ()
  "Book1 is a list of one or more rows."
  (=one-or-more (=row)))

