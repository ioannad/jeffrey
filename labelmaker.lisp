(in-package :jeffrey.labelmaker)

(defun fancy-dir ()
  (concatenate 'string 
	       *local-directory*
	       "fancy-labels/"))

(defun filename (name ending)  
  (concatenate 'string
	       (fancy-dir)
	       (format NIL "~a" name)
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

(defun make-all-texs ()
  (loop for name being the hash-keys of *graph*
     using (hash-value node)
     do (make-tex-file name node)))

(defun make-fancy-labels ()
  (make-all-texs)
  (run "/bin/bash" '("make-fancy-labels.sh") :error T :output T))
			
