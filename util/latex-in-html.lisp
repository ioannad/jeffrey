;;; # Package :jeffrey.latex-in-html
;;;

(in-package :jeffrey.latex-in-html)
 
;;;
;;; ## Description
;;;
;;; This package converts the LaTeX-formatted statements of the forms
;;; `(node-latex node)`
;;; into HTML renderable versions, and prints a template for the package
;;; `HTML_template`, with a table of these statements.
;;; It only takes care of the LaTeX-symbols which appear in the
;;; statements of the forms in the Consequences of the Axiom of Choice
;;; project.
;;; One can easily add more symbols by adding pairs or triplets to
;;; `*character-pairs*` or `*{}-triplets*`, respectively.
;;;
;;; For my purposes, there are three styles of things to be transformed
;;; to html from LaTeX. The first two are stored in `*character-pairs*`
;;; and the last in `*{}-triplets`:
;;;
;;; * math snippets (LaTeX-formatted text enclosed in $, will be displayed
;;;   as a png file),
;;;
;;; * individual characters in standard text (e.g. "\\\"o" to "&ouml"), and
;;;
;;; * standard text in curly brackets with an identifier before or after 
;;;   the left curly bracket.
;;;
;;; ## Code
;;;
;;; ### Local storage
;;;
;;; A database to hold the html formatted statements:

(defvar *nodes-html* (make-hash-table))

(defun make-node-html (node-name statement) ;=> new entry in hash-table
  (if (gethash node-name *nodes-html*)
      (error "Node with name ~a has already been added, with statement: ~a~%"
	     node-name statement)
      (setf (gethash node-name *nodes-html*)
	    statement)))

(defun update-node-html (node-name new-statement) ;=> updates hash-table
  (if (gethash node-name *nodes-html*)
      (setf (gethash node-name *nodes-html*)
	    new-statement)
      (error "Node with name ~a not in the database.~%"
	     node-name)))
  
(defvar *database-filename*
  (concatenate 'string
	       *local-directory*
	       "nodes-html.db"))

(defun save-nodes-html () ;=> saves database in file
  (let ((list (loop for name being the hash-keys of *nodes-html*
		 using (hash-value statement)
		 collect (list name statement))))
    (with-open-file (out *database-filename*
			 :direction :output
			 :if-exists :supersede)
      (with-standard-io-syntax
	(print list out)))))

(defun load-nodes-html () ;=> loads database from file
  (let ((list '()))
    (with-open-file (in *database-filename*)
      (with-standard-io-syntax
	(setf list (read in))))
    (setf *nodes-html (make-hash-table))
    (loop for (name statement) in list
       do (setf (gethash name *nodes-html*) statement))))

(defun get-by-name (name)
  (list :name name :statement (gethash name *nodes-html*)))

(defun select-by-content (search-words) 
  (loop for node-name being the hash-keys of *nodes-html*
     using (hash-value statement)
     when (some (lambda (word)
		  (search word statement))
		search-words)
     collect (list :name node-name :statement statement)))

;;; Temporary storage for the '(math-snippet identifier) pairs:

(defvar *math-snippets* nil
  "Math-snippets are two-element lists, of one LaTeX-formatted
math string without the surrounding $s, and one unique number.")

;;; ### First transform parens outside of $...$ of the parameters
;;;
;;; The parameters all have their parens outside of $...$, which
;;; does not look good after processing.

(defun parameter-transform (statement)
  (if (and #1=(search "$)" statement)
	   #2=(search "}" statement)
	   (< #1# #2#))
      (let ((begin (subseq statement 0 #2#))
	    (rest  (subseq statement #2#)))
	(concatenate 'string
		     (jeffrey.process-strings:search-replace
		      "$)" ")}$" (jeffrey.process-strings:search-replace
				  "($" "$\\mathbf{(" begin))
		     rest))
      statement))

(assert (equal "{bla 5333 $\\mathbf{(foo)}$.} blablablalblablabla"
	       (parameter-transform "{bla 5333 ($foo$).} blablablalblablabla")))

;;; ### Extract math-snippets from statements.

(defun get-save-identifier (snippet)
  "Gets the identifier of a snippet, if one exists, otherwise
creates a new identifier, and pushes '(snippet identifier) to
*math-snippets*"
  (let ((found? (member snippet *math-snippets*
			:test #'equal :key #'first)))
    (if found? 
	(second (first found?))
	(let ((n (length *math-snippets*)))
	  (push (list snippet n) *math-snippets*)
	  n))))

(defun transform-math (statement start end snip-number)
  (concatenate
   'string
   (subseq statement 0 start)
   (format nil "<img src=\"math-snippets/snip-~a.png\">"
	   snip-number)
   (find-save-transform-math (subseq statement (+ 1 end)))))

(defun find-save-transform-math (statement) ;=> html-text
  (let ((start (position #\$ statement)))
    (if start
	(let* ((snip-length (position #\$
				      #1=(subseq statement
						 (+ start 1))))
	       (snip (string-trim maxpc.char:*whitespace*
				  (subseq #1# 0 snip-length)))
	       (number (get-save-identifier snip)))
	  (transform-math statement
			  start
			  (+ start snip-length 1)
			  number))
	statement)))

(defun find-save-transform-maths () ;=> updates *nodes-html*
  (loop for node being the hash-values of *graph*
     using (hash-key name)
     for statement = (parameter-transform (node-LaTeX node))
     do (print name)
     do (print statement)
     do (make-node-html name (find-save-transform-math statement))))
	     
;;; ### Create a .tex and a .png file for each unique snippet

(defun snippet->tex (stream snip)
  (format
   stream "
\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\documentclass[tikz,xcolor]{standalone}
\\usepackage{amsmath,amssymb,color,xcolor}
\\newcommand{\\Cal}{\\mathcal}
\\begin{document}
\\begin{tikzpicture}
\\node[minimum size = 16pt] at (0,0) {$~a$};
\\end{tikzpicture}
\\end{document}"
   snip))

(defun snippet-tex-file (number)
  (concatenate 'string
	       jeffrey.main:*local-directory*
	       (format nil "math-snippets/snip-~a.tex" number)))
	       
(defun make-snippet-texs ()
  (loop for (snip number) in *math-snippets*
     for filename = (snippet-tex-file number)
     do (with-open-file (out filename
			     :direction :output
			     :if-exists :supersede)
	  (snippet->tex out snip))))

;;; To create the .png files I wrote the bash script make-math-snippets.sh

(defun make-snippets ()
  (print "Finding snippets in statements")
  (find-save-transform-maths)
  (print "Making the tex files")
  (make-snippet-texs)
  (print "Running the bash script make-math-snippets.sh")
  (external-program:run "/bin/bash" '("./make-math-snippets.sh")
			:error T :output T))

;;; ### Adjust the standard text of each statement			
;;;
;;; Set the translations of individual characters:

(defvar *character-pairs* nil
    "Translation pairs of the form '(latex html)")

(setf *character-pairs*
      (list (list "\\\"o"     "&ouml;")
	    (list "\\\"a"     "&auml;")
	    (list "\\'e"      "&eacute;")
	    (list "\\L "      "&#321;")
	    (list "\\'s"      "&#347;")
	    (list "\\newline" "")))

;;; Set the translations of standard text in curly brackets:

(defvar *{}-triplets* 
  "Translation triplets of the form '(latex-start html-start html-end)")

(setf *{}-triplets*
  (list (list "{HR"        "<strong>"                    "</strong>")
	(list "{\\it"       "<i>"                         "</i>")
	(list "\\ac{"       "see <font class=\"author\">" "</font>")
	(list "\ac{"        "see <font class=\"author\">" "</font>")
	(list "\\cite{"     "<font class=\"year\">"       "</font>")
	(list "\cite{"     "<font class=\"year\">"       "</font>")
	(list "\\itemitem{" "<font class=\"enum\">"       "</font>")
	(list "\\icopy{"    "<font class=\"icopy\">"      "</font>")
	(list "\\hbox{"     ""                            "")))

;;; Use the translations to adjust the statements for printing in html:

(defun translate-character (statement translation-pair)
  (destructuring-bind (this that) translation-pair
    (jeffrey.process-strings:search-replace this that statement)))
  
(defun translate-{} (statement translation-triplet)
  (destructuring-bind (latex-start html-start html-end) translation-triplet
    (let ((start (search latex-start statement)))
      (if start
	  (let ((end (position #\} (subseq statement start))))
	    (format t "start: ~a, end: ~a~%" start end)
	    (concatenate 'string
			 (subseq statement 0 start)
			 html-start
			 (subseq statement
				 (+ start (length latex-start))
				 (+ start end))
			 html-end
			 (subseq statement (+ start end 1))))
	  statement))))

(defun adjust-characters (statement)
  (reduce #'translate-character *character-pairs*
	  :initial-value statement))

(defun adjust-{} (statement)
  (reduce #'translate-{} *{}-triplets*
	  :initial-value statement))

(defun adjust-statement (statement)
  (adjust-characters (adjust-{} statement)))
  
(defun adjust-statements ()
  (loop for name being the hash-keys of *nodes-html*
     using (hash-value statement)
     do (update-node-html name (adjust-statement statement))))

;;; ### Create a template with the list of forms and their statements
(defvar *template-file* (concatenate 'string
				     *local-directory*
				     "www/forms-statements.tmpl"))

(defun create-html-template ()
  (let ((checkforms (loop for i from 0 to 430
		       unless (member i *bad-forms*)
		       collect (get-by-name i)))
	(*string-modifier* #'identity))
    (with-open-file (out *template-file*
			 :direction :output
			 :if-exists :supersede)
      (format out
	      "~a~%"
	      (with-output-to-string (stream)
		(html-template:fill-and-print-template
		 #P"names-and-statements.tmpl"
		 (list :checkforms checkforms)
		 :stream stream))))))

;;; ### The whole process:

(defun make-files-and-database ()
  (print "Reseting variables...")
  (setf *math-snippets* nil)
  (setf *nodes-html* (make-hash-table))
  (print "Making snippets...")
  (make-snippets)
  (print "Adjusting statements...")
  (adjust-statements)
  (print "Saving html statements of nodes...")
  (save-nodes-html)
  (print "Creating HTML_Template template...")
  (create-html-template))

;;; After evaluating (make-files-and-database), the function
;;; (load-nodes-html) can be used from website.lisp to
;;; populate the database. 
