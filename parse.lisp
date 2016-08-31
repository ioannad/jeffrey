(in-package :jeffrey.parse)

(defvar *comments* NIL) ;; consider storing comments here (still unused).

#|
Parse form information:
|#

(defun ?form-delimiter ()
  (?seq (?string "\\medskip")
	(?char #\Newline)))
    
(defun ?eq-form-delimiter ()
  (?seq (?string "\\smallskip")
	(?char #\Newline)))

(defun ?delimiter ()
  (%or (?eq-form-delimiter)
       (?form-delimiter)))

(assert (multiple-value-bind (a b c)
	    (parse "\\medskip
" (?delimiter))
	  (and (equal a NIL)  ;; returns nothing
	       (equal b T)    ;; succeeds in parsing a delimiter
	       (equal c T)))) ;; finished with the input

(assert (multiple-value-bind (a b c)
	    (parse "\\smallskip
 mnmn" (?delimiter))
	  (and (equal a NIL)    ;; returns nothing
	       (equal b T)      ;; succeeds in parsing a delimiter
	       (equal c NIL)))) ;; not finished with the input

(defun =text-until (parser)
  "The input, which `parser` parses is not consumed."
  (=subseq (%some (?not parser))))

(defun =parameter ()
  (let ((start (?char #\())
	(end   (?char #\))))
    (=subseq (?seq start (=text-until end) end))))
  
(assert (equal "($p$)" (parse "($p$)lg" (=parameter)))) 
(assert (equal "(n)" (parse "(n)lg" (=parameter)))) 
(assert (equal "($\\alpha$)" (parse "($\\alpha$)lg" (=parameter)))) 

(defun =name ()
  (=destructure (number _ parameter _)
      (=list (=natural-number)
	     (%maybe (?whitespace))
	     (%maybe (=parameter))
	     (?string ".}"))
    (list number parameter)))

(assert (equal '(423 "($n$)") (parse "423 ($n$).}hjfjf" (=name))))
(assert (equal '(423 NIL) (parse "423.}  oiiu ghj f" (=name))))

(defun =short-name-ending ()
  (=subseq (=list (?eq #\$)
		 (%maybe (?eq #\)))
		 (?eq #\:)
		 (?whitespace))))

(assert (equal "$): " (parse "$): " (=short-name-ending))))
(assert (equal "$: " (parse "$: " (=short-name-ending))))
  
(defun =short-name ()
  (=destructure (short-name ending)
      (=list (=text-until (=short-name-ending))
	     (=short-name-ending))
    (concatenate 'string short-name ending)))

(assert (equal
	 (parse
	  " $UT(WO,\aleph_{0},WO)$ ($U_{\aleph_{1}}$): The un"
	  (=short-name))
	 " $UT(WO,aleph_{0},WO)$ ($U_{aleph_{1}}$): "))

(defun reference-start-strings ()
  (list "(See" "See" "van \\ac" "\\item{}\\ac" "\\ac" "Note" "Clear" "G\\."))

(defun reference-start-p (x)
  (member x (reference-start-strings) :test #'equal))

(assert (reference-start-p "\\ac"))

(defun =reference-start ()
  (=subseq (%some (apply '%or (mapcar '?string (reference-start-strings))))))

(assert (equal "Note" (parse "Note" (=reference-start))))

(defun =full-name ()
  (=text-until (%or (?delimiter)
		    (=reference-start))))

(assert (equal "relatively prime. "
	       (parse "relatively prime. See form" (=full-name))))
(assert (equal "relatively prime$.
"
	       (parse "relatively prime$.
\\smallskip
" (=full-name))))

(defun =references ()
  (=destructure (ref-start ref)
      (=list (=reference-start)
	     (=text-until (?delimiter)))
    (concatenate 'string ref-start ref)))

(assert (equal "See form 218 and \\ac{Bleicher} \\cite{1964}. 
"
	       (parse "See form 218 and \\ac{Bleicher} \\cite{1964}. 
\\medskip
"
		      (=references))))

(defun =main-form ()
  (=destructure (_ _ name short-name full-name references)
      (=list (?form-delimiter)
	     (?string "\\noindent{\\bf FORM ")
	     (=name)
	     (%maybe (=short-name))
	     (=full-name)
	     (%maybe (=references)))
  (list name short-name full-name references)))

(assert (equal
	 '((6 NIL)
	   "a$: "
	   "b. "
	   "G\\. \\ac{Moore} \\cite{1982} and note 3.
")
	 (parse "\\medskip
\\noindent{\\bf FORM 6.}a$: b. G\\. \\ac{Moore} \\cite{1982} and note 3.
\\smallskip
"
		(=main-form))))

(defun =eq-name ()
  (=destructure (number _ eq-id _ parameter _)
      (=list (=natural-number)
	     (?whitespace)
	     (=subseq (%some (?satisfies 'upper-case-p)))
	     (%maybe (?whitespace))
	     (%maybe (=parameter))
	     (%or (?string "]}")
		  (?string "].}")))
    (list number eq-id parameter)))
    
(assert (equal '(14 "M" NIL) (parse "14 M]}  R. Cowen's " (=eq-name))))
(assert (equal '(14 "N" "(n)") (parse "14 N(n)]} ($n\\in\\omega$" (=eq-name))))
(assert (equal '(430 "AB" "($p$)") (parse "430 AB($p$)].} (Where $p" (=eq-name))))


(defun =eq-form ()
  (=destructure (_ _ eq-name short-name full-name references)
      (=list (?eq-form-delimiter)
	     (?string "\\item{}{\\bf [")
	     (=eq-name)
	     (%maybe (=short-name))
	     (=full-name)
	     (%maybe (=references)))
    (list eq-name short-name full-name references)))

(assert (equal
	 '((14 "M" NIL) NIL
	   "  R. Cowen's ... T.  "
	   "\\ac{Cowen} ...
21. \\iput{Konig's lemma}
")
	 (parse "\\smallskip
\\item{}{\\bf [14 M]}  R. Cowen's ... T.  \\ac{Cowen} ...
21. \\iput{Konig's lemma}
\\smallskip
\\item{}{\\bf [14 N(n)]}..." (=eq-form))))

(defun =form ()
      (=list (=main-form)
	     (%maybe (%some (=eq-form)))))

(assert (equal
	 '(((430 "($p$)") " (Where $p$ is a prime) AL21$(p)$: "
	    "Every ... $V = S \\oplus
S'$.  "
	    "\\ac{Bleicher} ... AL21}.
")
	   (((430 "A" "($p$)")
	      " (Where $p$ is a prime) MC$...$, MC4$(p)$: "
	      " For ... prime. "
	      "See ... 6.38}.")))
	 (parse "\\medskip
\\noindent{\\bf FORM 430($p$).} (Where $p$ is a prime) AL21$(p)$: Every ... $V = S \\oplus
S'$.  \\ac{Bleicher} ... AL21}.
\\smallskip
\\item{}{\\bf [430 A($p$)].} (Where $p$ is a prime) MC$...$, MC4$(p)$:  For ... prime. See ... 6.38}." (=form))))

(assert (equal
	 '(((0 NIL) NIL " $0 = 0$.
" NIL) NIL)
	 (parse "\\medskip
\\noindent{\\bf FORM 0.} $0 = 0$.
\\medskip
"
		(=form))))

(defun =formsnum.tex ()
  (=list (=text-until (?form-delimiter))
	 (%some (=form))))
#|
(assert (equal
	 ""
	 (parse "bla \\medskip
\\noindent{\\bf FORM 0.} $0 = 0$.
\\smallskip
\\item{}{\\bf [0 A]}  Cardinal successors 2:  For n)$.
\\ac{Tarski} \\cite{1954a} and \\ac{Jech} \\cite{1966a}.
\\smallskip
\\item{}{\\bf [0 AK]} Every separable metric space is second countable.

\\medskip
\\medskip
\\noindent{\\bf FORM 1.} $C(\\infty,\\infty)$:  The Axiom of Choice:
Every  set  of  non-empty sets has a choice function.
\\rightheadtext{Form 1: The Axiom of Choice}
\\iput{axiom of choice}
\\smallskip
\\item{}{\\bf [1 A]} In every vector space, every generating set
contains a basis.  \\ac{Halpern} \\cite{1966}.
\\medskip
\\medskip
\\noindent{\\bf FORM 1.} $C(\\infty,\\infty)$:  The Axiom of Choice:
Every  set  of  non-empty sets has a choice function.
\\rightheadtext{Form 1: The Axiom of Choice}
\\iput{axiom of choice}
\\smallskip
\\item{}{\\bf [1 A]} In every vector space, every generating set
contains a basis.  \\ac{Halpern} \\cite{1966}.
"
		(=formsnum.tex))))

|#
		 
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

(defun =skip-whitespace (parser)
  (=destructure (_ result)
      (=list (%maybe (%some (?whitespace)))
	     parser)))

(defun =field ()
  "A field is a code number for the implication 'Form #row' => 
'Form-#column', where #row or #column is the number of the row or 
column respectively."
  (=skip-whitespace (=natural-number)))

(defun =row-delim ()
  "Book1 ends rows with a -1"
  (=skip-whitespace (?string "-1")))

(defun =row ()
  "A row is a list of one or more fields that ends in -1."
  (=destructure (result _)
      (=list (%some (=field)) (=row-delim))))

(assert (equal '(1 2 3) (parse "1 2 3 -1" (=row))))

(defun =book1 ()
  "Book1 is a list of one or more rows."
  (%some (=row)))

(assert (equal (parse "1 2 3 -1 
4 5 6 -1   7 8 9 -1"
		      (=book1))
	       '((1 2 3) (4 5 6) (7 8 9))))

