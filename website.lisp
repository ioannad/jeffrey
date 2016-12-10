(in-package :jeffrey.website)

;; Server and initial dispatch table

(defvar *server*)


(setq hunchentoot:*dispatch-table*
      (list (static "/logo.jpg"    "www/logo.jpg")
	    (static "/jeffrey.css" "www/jeffrey.css")
	    (hunchentoot:create-prefix-dispatcher
	     "/choiceless-grapher.htm" 'generate-index-page)
	    (hunchentoot:create-prefix-dispatcher
	     "/diagram.htm" 'diagram)))


(defmacro static (uri rel-filepath)
  `(hunchentoot:create-static-file-dispatcher-and-handler
    ,uri
    (concatenate 'string *local-directory* ,rel-filepath)))

(defun start-website (&key (address "localhost") (port 8080))
  (setq *server* (hunchentoot:start
		  (make-instance 'easy-acceptor
				 :address address
				 :port port))))

(defun stop-website ()
  (stop *server*))

;; HTML pages

(setq html-template:*default-template-pathname*
      (concatenate 'string *local-directory* "www/"))

(defun generate-index-page ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"jeffrey.tmpl"
     (list :title "Choiceless Grapher"
	   :index? T)
     :stream stream)))

;; fast solution to the problem of partial parses
;; (in case of "1 24 5 bla 23" input):
(defun parse-input (input-string)
  (when (every (lambda (x) (or (digit-char-p x)
			       (equal #\Space x)
			       (equal #\Tab x)))
	       input-string)
    (sort (mapcar #'parse-integer
		  (split-sequence #\Space input-string))
	  #'<)))

(defun encode-names (input-names)
  "This code is not a code, it's literal and too long."
  (format nil "~{~a~^-~}" input-names))

(defun encoded-string (input-string)
  (list #1=(parse-input input-string)
	#2=(encode-names #1#)
	#3=(concatenate 'string "/" #2# ".png")
	(concatenate 'string "diagrams" #3#)))

(defun diagram ()
  (let ((input-string (parameter "names")))
    (destructuring-bind (input-names filename uri rel-path)
	(encoded-string input-string)
      (progn (graph input-names filename "fancy" "png")
	     (push (static uri rel-path)
		   hunchentoot:*dispatch-table*)
	     (with-output-to-string (stream)
	       (html-template:fill-and-print-template
		#P"jeffrey.tmpl"
		(list :title "Implication diagram"
		      :index? NIL
		      :print-names input-string
		      :path-to-image uri)
		:stream stream))))))
  
