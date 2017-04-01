;; # IO/dot package
;;
;; All related to dot code goes here.

(defun print-dot-head ()
  "Writes the standard header of a diagram to the {*standard-output*}."
  (format *standard-output* 
	  "digraph diagram { ~%
          ratio=0.5;~%
	  node [shape=ellipse,width=0.5,height=1];~%"  ))

(defun print-fancy-label (name)
  "Takes the node `name` from the user input and returns a string 
that makes Graphviz-dot use the pdf file in the folder 
`\"form-name-pics\"`, that contains this form's LaTeX formatted 
statement." 
  (format *standard-output*
	  "~a [image=\"~afancy-labels/~a.png\", label=\" \"];~%"
	  name *local-directory* name))

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
