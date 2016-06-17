(in-package :jeffrey.draw)

#|
example of expected use of exported function: 
(draw '(2 3 65 43 22 102) "diagram-name")
This should draw the diagram between the nodes with names in the 
above list, and save the .dot and .pdf files to 
~/quicklisp/local-projects/jeffrey/diagrams/diagram-name.dot and 
...pdf respectively.
|#

(defun make-filename  (postfix-ending input-filename)
  (concatenate 'string
	       *local-directory*
	       "diagrams/"
	       input-filename
	       postfix-ending))

(defun print-dot-head ()
  "Writes the standard header of a diagram to the {*standard-output*}."
  (format *standard-output* 
	  "digraph diagram { ~%
	  node [shape=ellipse,width=0.5,height=1];~%"  ))

(defun print-fancy-label (name)
  "Takes the node `name` from the user input and returns a string 
that makes Graphviz-dot use the pdf file in the folder 
`\"form-name-pics\"`, that contains this form's LaTeX formatted 
statement." 
  (format *standard-output*
	  "~a [image=\"fancy-labels/~a.png\", label=\" \"];~%"
	  name name))

(defun special-join-string-list (string-list)
  "Formats the parameters, which may appear in the number labels,
in unicode instead of LaTeX."
  (let ((left (first string-list))
	(parameter (second string-list))
	(right (third string-list)))
    (if parameter
	(format nil "~a~a~a"
		left
		(cond ((equal parameter "\\alpha")
		       "α")
		      ((equal parameter "\\epsilon")
		       "ε")
		      (T parameter))
		right)
	left)))
	
		  

  
(defun number-label (node)
  "Takes the node `name` and returns a string with a simple `HR name`
of the node with this `name`. Name parameters are shown."
  (let* ((latex  (node-LaTeX node))
	 (n      (search ".}" latex))
	 (label% (subseq latex 1 n)))
    (special-join-string-list (split-sequence #\$ label%))))
	 
(defun print-number-label (name node)
  "Takes the node `name` and returns a string in dot-syntax, with
the `(number-label name)`."
  (format *standard-output* 
	  "~a [label=\"~a.\"];~%"
	  name (number-label node)))

(defun print-node-label (name node style)
  "Writes the labels for the nodes in the style `style`, depending on
the user input. The user as two choices for a style: 

* `\"fancy\"` creates diagrams with the full LaTeX statements of the 
forms, which are stored as pictures in png format, in the folder 
`diagrams/form-name-pics`. This is the default. 
* `\"numbers\"` creates plain diagrams with only 'HR `name`' style 
nodes."
  (if(equal style "fancy")
     (print-fancy-label name)
     (if (equal style "numbers")
	 (print-number-label name node)
	 (error "Wrong style given in print-dot-file: ~a .~%" 
		style))))

(defun print-bold-arrow (name-i name-j)
  "Prints a boldfaced arrow in dot-syntax from `name-i` to `name-j` 
to the `*standard-output*`."
  (format *standard-output* 
	  "~a -> ~a [style=bold];~%"
	  name-i 
	  name-j))

(defun print-gray-arrow (name-i name-j)
  (format *standard-output* 
	  "~a -> ~a [color=dimgray];~%"
	  name-i
	  name-j))

(defun print-arrows-from (name node input-names)
  "The loop inside the loop of `draw-dot-content.`"  
  (loop for name% in input-names
     for node% = (call name%)
     when (not (equal name name%))
     do (cond ((and (implies-p     node node%)
		    (implies-not-p node% node))
	       (print-bold-arrow name name%))
	      ((and (implies-p node node%)
		    (not (implies-not-p node% node)))
	       (print-gray-arrow name name%))
	      (T NIL))))

(defun print-dot-content (input-names style)
  (loop for name in input-names
     for node = (call name)
     do (print-node-label name node style)
       (print-arrows-from name node input-names)))

(defun run-tred-dot (dot-file pdf-file)
  "Runs Graphviz tred on {dot-file} and feeds the output to 
Graphviz dot. Outputson {pdf-file}, which contains the diagram."
  (sb-ext:run-program "/usr/bin/tred"
		      (list dot-file " | dot -Tpdf -o " pdf-file)))
		     
(defun draw (input-names diagram-filename style)
  (let ((dot-file      (make-filename ".dot" diagram-filename))
	(pdf-file      (make-filename ".pdf" diagram-filename)))
    (with-open-file (*standard-output* dot-file
				       :direction :output
				       :if-does-not-exist :create
				       :if-exists :supersede)
      (print-dot-head)
      (print-dot-content input-names style)
      (format *standard-output* "~%}"))
    (run-tred-dot dot-file pdf-file)
    (format nil
	    "Files ~a and ~a in diagrams/ should be created successfully."
	    dot-file pdf-file)))
