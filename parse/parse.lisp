(in-package :jeffrey.parse)

#|
Parse form information:
|#

(defun ?tex-skip ()
  (?seq (%or (?string "\\medskip")
	     (?string "\\smallskip"))
	(?char #\Newline)))

(defun ?form-delimiter ()
  (?seq (%some (?tex-skip))
	(?string "\\noindent{\\bf FORM ")))
    
(defun ?eq-form-delimiter ()
  (?seq (?tex-skip)
	(?string "\\item{}{\\bf ")))

(defun ?delimiter ()
  (%or (?eq-form-delimiter)
       (?form-delimiter)))

(defun test-delimiters ()
  (assert (multiple-value-bind
		(a b c) (parse (test-string 0) (?delimiter))
	    (and (equal a NIL)  ;; returns nothing
		 (equal b T)    ;; succeeds in parsing a delimiter
		 (equal c T)))) ;; finished with the input
  
  (assert (multiple-value-bind (a b c)
	      (parse (test-string 1) (?delimiter))
	    (and (equal a NIL)    ;; returns nothing
		 (equal b T)      ;; succeeds in parsing a delimiter
		 (equal c NIL))))) ;; not finished with the input
  
(defun =text-until (parser)
  "The input, which `parser` parses is not consumed."
  (=subseq (%some (?not parser))))

(defun =name ()
  (flet ((name-end () (?string "}")))
    (=destructure (name _)
	(=list (=text-until (name-end))
	       (name-end)))))

(defun test-name ()
  (assert (equal "423 ($n$)." (parse "423 ($n$).}hjfjf" (=name))))
  (assert (equal "423." (parse "423.}  oiiu ghj f" (=name)))))
  
#| Perhaps use the following in a subsequent parse:

(defun =short-name-ending ()
  (=subseq (=list (?eq #\$)
		 (%maybe (?eq #\)))
		 (?eq #\:)
		 (?whitespace))))
  
(defun =short-name ()
  (=destructure (short-name ending)
      (=list (=text-until (=short-name-ending))
	     (=short-name-ending))
    (concatenate 'string short-name ending)))

(defun test-short-name ()
  (assert (equal "$): " (parse "$): " (=short-name-ending))))
  (assert (equal "$: " (parse "$: " (=short-name-ending))))
  (assert (equal
	   (parse
	    " $UT(WO,\aleph_{0},WO)$ ($U_{\aleph_{1}}$): The un"
	    (=short-name))
	   " $UT(WO,aleph_{0},WO)$ ($U_{aleph_{1}}$): ")))|#

(defun reference-start-strings ()
  (list "(See" "See" "van \\ac" "\\item{}\\ac" " \\ac" "
\\ac" "Note" "Clear" "G\\." "
Keremedis "))

(defun reference-start-p (x)
  (member x (reference-start-strings) :test #'equal))

(defun =reference-start ()
  (=subseq (%some (apply '%or (mapcar '?string (reference-start-strings))))))

(defun =references ()
  (=destructure (ref-start ref)
      (=list (=reference-start)
	     (=text-until (?delimiter)))
    (concatenate 'string ref-start ref)))

(defun test-references ()
  (assert (reference-start-p "\\ac"))
  (assert (equal "Note" (parse "Note" (=reference-start))))
  (assert (equal (test-string 3)
		 (parse (test-string 2)
			(=references)))))

(defun =full-name ()
  (=text-until (%or (?delimiter)
		    (=reference-start))))

(defun test-full-name ()
  (assert (equal "relatively prime. "
		 (parse "relatively prime. See form"
			(=full-name))))
  (assert (equal (test-string 4)
		 (parse (test-string 5)
			(=full-name)))))  

(defun =main-form ()
  (=destructure (_ name full-name references)
      (=list (?form-delimiter)
	     (=name)
	     (=full-name)
	     (%maybe (=references)))
    (list name full-name references)))

(defun test-main-form ()
  (assert (equal
	   '("6." "a$: b. "
	     "G\\. \\ac{Moore} \\cite{1982} and note 3.")
	   (parse (test-string 6)
		  (=main-form)))))

(defun =eq-form ()
  (=destructure (_ eq-name full-name references)
      (=list (?eq-form-delimiter)
	     (=name)
	     (=full-name)
	     (%maybe (=references)))
    (list eq-name full-name references)))

(defun test-eq-form ()
  (assert (equal
	   (list "[14 M]" "  R. Cowen's ... T.  "
		 (test-string 8))
	   (parse (test-string 7) (=eq-form)))))
  
(defun =form ()
  "The main form and its equivalents are returned in one list, starting with the main form."
  (=destructure (main equivalents)
      (=list (=main-form)
	     (%maybe (%some (=eq-form))))
    (append (list main) equivalents)))

(defun test-form ()
  (assert (equal
	   '(("430($p$)."  " A " "\\ac{B}")
	     ("[430 A($p$)]." " C " "See V"))
	   (parse (test-string 9) (=form))))
  (assert (equal
	   '(("0." " $0 = 0$." NIL))
	   (parse (test-string 10) (=form)))))

(defun no-newlines-subform (subform)
  "This function replaces newlines with spaces, at the full-name 
and references of `subform`. With `subform` I mean either 
main-form or eq-form (an equivalent form). No further processing at this point, the rest will be done in a string processing package."
  (cons (first subform) ;; the first is the name 
	(mapcar (lambda (string)
		  (substitute #\Space #\Newline string))
		;; the rest are the strings with possible newlines
		(rest subform)))) 

(defun no-newlines (forms)
  "The argument, `forms`, is a list of forms, which are lists of 
subforms"
  (mapcar (lambda (form) (mapcar #'no-newlines-subform form))
	  forms))

(defun =formsnum.tex ()
  "*Description:*

   A MaxPC parser, which parses the form information, i.e., name, 
   LaTeX-statement, and references from the TeX-file 
   {Howard-Rubin-data/FORMSNUM.TEX}."
  (=destructure (_ forms)
      (=list (=text-until (?form-delimiter))
	     (%some (=form)))
    (no-newlines forms)))
   
  
(defun test-formsnum.tex ()
  (assert (equal (parse (test-string 11) (=formsnum.tex))
		 '((("0."     " A "     NIL)
		    ("[0 B]"  " C "     "\\ac{ D ")
		    ("[0 EF]" " G "     NIL))
		   (("1."     " H$: I " NIL)
		    ("[1 A]"  " J "     "See K ")))))
  (assert (equal
	   (parse (test-string 12)
		  (=formsnum.tex))
	   (formsnum-test-result))))

(defun test-formsnum-parsers ()
  "*Description:*

   Tests all the parsers, including the non-exported ones."
  (test-delimiters)
;  (test-parameter)
  (test-name)
  (test-references)
  (test-full-name)
  (test-main-form)
 ; (test-eq-name)
  (test-eq-form)
  (test-form)
  (test-formsnum.tex))


;;; ### Parse book1

;;; The functions defined here convert the matrix that is stored in
;;; book1 of Howard and Rubin's "Consequences of the Axiom of
;;; Choice" into a graph structure. 


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
  "*Description:* 

   A MaxPC parser, which parses the implication information from 
   the file {Howard-Rubin-data/book1}. Note that a lot of this 
   information is later dropped (codes 2, 4, 5, and 6), to be
   recalculated using the package {:jeffrey.predicates}."
  (%some (=row)))

(assert (equal (parse "1 2 3 -1 
4 5 6 -1   7 8 9 -1"
		      (=book1))
	       '((1 2 3) (4 5 6) (7 8 9))))


;;; ### Parse input for website

(defun =word ()
  (=subseq (%some (?not (?whitespace)))))

(assert (equal "bla" (parse "bla bla" (=word))))

(defun =names ()
  (%some (=skip-whitespace (=natural-number))))

(assert (equal '(1 2 33) (parse "1 2 33" (=names))))










;; ----------- Testing strings


(defun test-string (x)
  (case x
    (0
      "\\medskip
\\medskip
\\noindent{\\bf FORM ")

     (1
      "\\smallskip
\\item{}{\\bf [ mnmn")

     (2
      "See form 218 and \\ac{Bleicher} \\cite{1964}. 
\\smallskip
\\item{}{\\bf [")

     (3
      "See form 218 and \\ac{Bleicher} \\cite{1964}. 
")

     (4
      "relatively prime$.
")

     (5
      "relatively prime$.
\\smallskip
\\item{}{\\bf [")

     (6
      "\\medskip
\\noindent{\\bf FORM 6.}a$: b. G\\. \\ac{Moore} \\cite{1982} and note 3.\\smallskip
\\item{}{\\bf [")

     (7
      "\\smallskip
\\item{}{\\bf [14 M]}  R. Cowen's ... T.  \\ac{Cowen} ...
21. \\iput{Konig's lemma}
\\smallskip
\\item{}{\\bf [14 N(n)]}...")

     (8
      "\\ac{Cowen} ...
21. \\iput{Konig's lemma}
")

     (9
      "\\medskip
\\noindent{\\bf FORM 430($p$).} A \\ac{B}\\smallskip
\\item{}{\\bf [430 A($p$)].} C See V")

     (10
      "\\medskip
\\noindent{\\bf FORM 0.} $0 = 0$.\\smallskip
\\item{}{\\bf [")

     (11
      "bla 
\\medskip
\\noindent{\\bf FORM 0.} A
\\smallskip
\\item{}{\\bf [0 B]} C \\ac{ D
\\smallskip
\\item{}{\\bf [0 EF]} G
\\medskip
\\noindent{\\bf FORM 1.} H$: I
\\smallskip
\\item{}{\\bf [1 A]} J See K
")

     (12
      "bla \\medskip
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
")))

(defun formsnum-test-result ()
  '((("0." " $0 = 0$. " NIL)
     ("[0 A]"
      "  Cardinal successors 2:  For n)$. "
      "\\ac{Tarski} \\cite{1954a} and \\ac{Jech} \\cite{1966a}. ")
     ("[0 AK]"
      " Every separable metric space is second countable. "
      NIL))
    (("1."
      " $C(\\infty,\\infty)$:  The Axiom of Choice: Every  set  of  non-empty sets has a choice function. \\rightheadtext{Form 1: The Axiom of Choice} \\iput{axiom of choice} "
      NIL)
     ("[1 A]"
      " In every vector space, every generating set contains a basis.  "
      "\\ac{Halpern} \\cite{1966}. "))))

   
