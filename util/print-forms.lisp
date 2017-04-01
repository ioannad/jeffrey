#|
Functions to print statements and forms in a LaTeX file.
|#

(in-package :jeffrey.read)

(defun print-forms-file-heading (names stream)
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
   This is a list of the consequences of the axiom of choice, which contains the ``Howard-Rubin'' (HR) numbers: ~a" names)
  (format stream "their statements, and references to the locations in which they were found by Paul Howard and Jane Rubin. The references and more information can be found in the book {\\bf Consequences of the Axiom of Choice}. All information has been extracted from the file ``FORMSNUM.TEX'' which was kindly provided by Paul Howard.\\~%
\\begin{itemize}~%"))

(defun print-subform (subform stream)
  (let ((LaTeX (second subform))
	(references (third subform)))
    (format stream "\\item~a ~%" LaTeX)
    (if (not (equal references ""))
	(format stream "{\\bf References:} ~a~%" references)
	NIL)))

(defun print-forms-file-ending (stream)
  (format stream "\\end{itemize}~%\\end{document}"))

(defun forms-and-equivalents () ;=> hash-table
  (let ((forms (make-hash-table))
	(forms-list (read-formsnum *forms-file*)))
    (loop for form in forms-list
       for name = (first (first form))
       do (setf (gethash name forms) form))
    forms))

(defun print-forms-to-file (names filename)
  "Creates or overwrites {filename}.tex with a LaTeX document with 
the form statements and references, to make sure that the forms 
have been parsed correctly."
  (let ((file (concatenate 'string filename ".tex"))
	(forms (forms-and-equivalents)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede)
      (print-forms-file-heading names stream)   
      (loop for name in names
	 for form = (gethash name forms)
	 do (loop for subform in form
	       do (print-subform subform stream)))
      (print-forms-file-ending stream))))

(defun statement-contains (words)
  (lambda (subform)
    (some (lambda (word) (search word (second subform)))
	  words)))

(defun print-forms-to-file-if (predicate filename)
   (let ((file (concatenate 'string filename ".tex"))
	 (forms (forms-and-equivalents)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede)
      (print-forms-file-heading '() stream)
      (loop for form being the hash-values of forms
	 do (loop for subform in form
	       if (funcall predicate subform)
	       do (print-subform subform stream)))
      (print-forms-file-ending stream))))

