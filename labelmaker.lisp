(in-package :jeffrey.labelmaker)

(defun filename (name ending)  
  (concatenate 'string 
	       *local-directory*
	       "diagrams/form-name-pics/"
	       (write-to-string name)
	       ending))



(defun tex-file-content (latex-statement width-cm)
  (format *standard-output*
	  "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
           \\documentclass[tikz,xcolor]{standalone}
           \\usepackage{amsmath,amssymb,color,xcolor}
           \\newcommand{\\Cal}{\\mathcal}
           \\newcommand{\\itemitem}[1]{\\newline\\hspace{5pt}{#1}\\hspace{5pt}}
           \\newcommand{\\icopy}[1]{\\textit{#1}}
           \\newcommand{\\ac}[1]{{#1}}
           \\renewcommand{\\cite}[1]{(#1)}
           \\renewcommand{\\medskip}{ }
           \\renewcommand{\\smallskip}{ }
           \\begin{document}
           \\begin{tikzpicture}
           \\node[text width=~acm,align=center] at (0,0) {~a};
           \\end{tikzpicture}
           \\end{document}"
	  width-cm
	  latex-statement))

(defun make-tex-file (name node)
  (with-open-file (*standard-output* (filename name ".tex")
			  :direction :output 
			  :if-exists :supersede)
    (let* ((latex-statement (node-latex node))
	   (width-cm        (if (> 150 (length latex-statement))
				3.5
				5.5)))
      (tex-file-content latex-statement width-cm))))

(defun make-png (name)
  "Creates a .png file for the node of the FORM, in the folder
diagrams/form-name-pics/."
  (sb-ext:run-program "/usr/bin/pdflatex" 
		      (list (filename name ".tex")))
  (sb-ext:run-program "/usr/bin/dvipng"
		      (list (filename name ".png")))
 
  (format t "File ~a created successfully.~%"
	  (filename name ".png")))
	   		
(defun make-all-texs ()
  (setf (node-LaTeX (gethash 360 *graph*)) "{HR 360.} A system of linear equations over the field $\\{0,1\\}$ has a solution, if and only if every finite subsystem has a solution.")
  (loop for name being the hash-keys of *graph*
     using (hash-value node)
     do (make-tex-file name node)))

(defun make-all-pngs ()
  (loop for name being the hash-keys of *graph*
     do (make-png name)))
	     