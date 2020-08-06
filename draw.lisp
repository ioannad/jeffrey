(in-package :jeffrey.draw)

#|
example of expected use of exported function:
(draw '(2 3 65 43 22 102) "diagram-name.png")
This should draw the diagram between the nodes with names in the
above list, and save the .dot and .png files to
~/quicklisp/local-projects/jeffrey/diagrams/diagram-name.dot and
...pdf respectively.
|#

(defun make-filename  (postfix-ending input-filename)
  (concatenate 'string
	       *local-directory*
	       "diagrams/"
	       input-filename
               "."
	       postfix-ending))

(defun print-dot-head ()
  "Writes the standard header of a diagram to the {*standard-output*}."
  (format *standard-output*
	  "digraph diagram { ~%
          ratio=0.5;~%
	  node [shape=ellipse,width=0.5,height=1];~%"  ))

(defun print-fancy-label (name)
  "Takes the node NAME from the user input and returns a string
that makes Graphviz-dot use the png file in the folder `fancy-labels`,
which contains this form's LaTeX formatted statement. Requires
(jeffrey.labelmaker::make-fancy-labels), which currently has issues
with `imagemagick`'s `convert`."
  (let ((fancy-label-filename (format nil
                                      "~afancy-labels/~a.png"
                                      *local-directory* name)))
    (if (probe-file fancy-label-filename)
        (format *standard-output*
                "~a [image=\"~a\", label=\" \"];~%"
                name fancy-label-filename)
        (error "~&Fancy labels seem not set up, or not set up properly.
Please evaluate (jeffrey.labelmaker::make-fancy-labels) and retry."))))

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
     (unless (equal style "numbers")
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

;; IO

(defun create-dot-file (dot-file input-names style)
  "DOT-FILE should be a complete pathname, with (the default) parent directory."
  (ensure-directories-exist dot-file :verbose t)
  (with-open-file (*standard-output* dot-file
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
    (print-dot-head)
    (print-dot-content input-names style)
    (format *standard-output* "~%}")))

(defun run-tred-dot (dot-file result-file ending)
  "Runs Graphviz tred on `dot-file` and feeds the output to Graphviz dot.
Outputs `result-file`, which contains the diagram. Requires the system
package Graphviz."
  (let ((command-string  (concatenate 'string
                                      "/usr/bin/tred "
                                      (namestring dot-file)
                                      " | dot -T"
                                      ending
                                      " -o "
                                      (namestring result-file))))
    (if (probe-file (pathname "/usr/bin/tred"))
        (run "/bin/sh"
             (list "-c" command-string)
             :output t)
        (error "Graphviz is not installed. Please install and retry."))))

(defun draw (input-names filename style ending)
  "`style` should be either the string \"fancy\" or \"numbers\".
`format` can be anything supported by dot, e.g., pdf, png, svg, ps.
If the DIAGRAM-FILENAME doesn't end in one of those formats, then png will be used."
  (let ((dot-file    (pathname (make-filename "dot" filename)))
        (result-file (pathname
                      (make-filename ending filename))))
    (handler-case
        (progn (create-dot-file dot-file input-names style)
               (run-tred-dot dot-file result-file ending))
      (:no-error (message value)
        (format *standard-output*
                "~&Files ~a and ~a in diagrams/ should be created successfully.~%"
                dot-file result-file)))))
