(in-package :jeffrey.draw)

#|
# Draw package

The main package of the drawing module.

Example of expected use of exported function: 
(draw '(2 3 65 43 22 102) "diagram-name")
This should draw the diagram between the nodes with names in the 
above list, and save the .dot and .pdf files to 
~/quicklisp/local-projects/jeffrey/<project-name>/diagrams/diagram-name.dot and 
...pdf respectively,
where <project-name> is the string value of :jeffrey.projects:*current-project*.
|#


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

(defun run-tred-dot (dot-file result-file ending)
  "Runs Graphviz tred on `dot-file` and feeds the output to 
Graphviz dot. Outputs `result-file`, which contains the diagram."
  (let ((command-string  (concatenate 'string 
				      "/usr/bin/tred " 
				      dot-file 
				      " | dot -T" 
				      ending
				      " -o "
				      result-file)))
    (run "/bin/sh" (list "-c" command-string)
			:output t)))


;; ## Exported functions

(defun draw (input-names diagram-filename style &optional (ending "png"))
  "`style` should be either the string \"fancy\" or \"numbers\".
`format` can be anything supported by dot, e.g., pdf, png, svg, ps.
The default format is pdf."
  (let ((dot-file    (make-filename ".dot" diagram-filename))
	(result-file (make-filename (concatenate 'string "." ending) diagram-filename)))
    (with-open-file (*standard-output* dot-file
				       :direction :output
				       :if-does-not-exist :create
				       :if-exists :supersede)
      (print-dot-head)
      (print-dot-content input-names style)
      (format *standard-output* "~%}"))
    (run-tred-dot dot-file result-file ending)
    (format nil
	    "Files ~a and ~a in diagrams/ should be created successfully."
	    dot-file result-file)))

(defun draw-descendants (names-list filename style &optional (ending "png"))
  (draw (name-transformer :descendants names-list)
	filename style ending))

(defun draw-ancestors (names-list filename style &optional (ending "png"))
  (draw (name-transformer :ancestors names-list)
	filename style ending))

(defun draw-random (amount-of-nodes filename style &optional (ending "png"))
  (draw (random-HR-numbers amount-of-nodes) filename style ending))

(defun draw-all-paths (from-name to-name filename style &optional (ending "png"))
  (draw (all-paths from-name to-name filename style ending)))

