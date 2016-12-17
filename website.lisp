(in-package :jeffrey.website)

;; Server and initial dispatch table

(defvar *server*)

(defun local (rel-path)
  (concatenate 'string *local-directory* rel-path))

(setq hunchentoot:*dispatch-table*
      (list (hunchentoot:create-static-file-dispatcher-and-handler
	     "/logo.jpg" (local "www/logo.jpg"))
	    (hunchentoot:create-folder-dispatcher-and-handler
	     "/examples/preview/" (local "examples/preview/"))
	    (hunchentoot:create-folder-dispatcher-and-handler
	     "/examples/" (local "examples/"))
	    (hunchentoot:create-static-file-dispatcher-and-handler
	     "/random-header.js"
	     (local "www/random-header.js"))
	    (hunchentoot:create-static-file-dispatcher-and-handler
	     "/jeffrey.css" (local "www/jeffrey.css"))
	    (hunchentoot:create-prefix-dispatcher
	     "/diagram" 'diagram)
	    (hunchentoot:create-prefix-dispatcher
	     "/examples" 'examples)
	    (hunchentoot:create-prefix-dispatcher
	     "/" 'generate-index-page)))

(defun start-website (&key (address "localhost") (port "8080"))
  (setq *server* (hunchentoot:start
		  (make-instance 'easy-acceptor
				 :address address
				 :port port))))

;; strangely, the above does not work for localhost on my side...
;; but the below does transmit the website fine at http://127.0.0.1:8080/
(defun start-website??  (&key (port "8080"))
  (setq *server* (hunchentoot:start
		  (make-instance 'easy-acceptor
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
	   :index? T
	   :bad-forms (format nil "~{~a~^, ~}" *bad-forms*))
     :stream stream)))

;double checks:
(defun parse-input (input-string)
  (when (every (lambda (x) (or (digit-char-p x)
			       (equal #\Space x)
			       (equal #\Tab x)))
	       input-string)
    (sort (mapcar #'parse-integer
		  (split-sequence #\Space input-string
				  :remove-empty-subseqs T))
	  #'<)))

(defvar *temp-hack* 0
  "Until I take care of concurrency, this ugly variable
will allow up to 100 large diagrams to be created in 'parallel'.")

(defun encode-names (input-names)
  "This code is not a code, it's literal and too long."
  (if (< (length input-names) 50)
      (format nil "~{~a~^-~}" input-names)
      (progn (if (>= *temp-hack* 100)
		 (setq *temp-hack* 0)
		 (setq *temp-hack* (+ 1 *temp-hack*)))
	     (eval (format nil "large-diagram-~a" *temp-hack*)))))

(defun encoded-string (key input-string label-style add-top-bottom)
  ;=> '(names-list filename uri rel-path)
  (let* ((input-names% (remove-duplicates
			(if (and add-top-bottom
				 (equal key :these))
			    (append '(0 1)
				    #1=(parse-input input-string))
			    #1#)))
	 (input-names (name-transformer key input-names%)))
    (list input-names
	  #2=(concatenate 'string
			  label-style "-"
			  (encode-names input-names))
	  #3=(concatenate 'string "/" #2# ".png")
	  (concatenate 'string "diagrams" #3#))))

(defmacro make-diagram-page (input-names uri)
  `(with-output-to-string (stream)
     (html-template:fill-and-print-template
      #P"jeffrey.tmpl"
      (list :title "Implication diagram"
	    :index? NIL
	    :print-names (format nil "~{~a~^, ~}"
				 ,input-names)
	    :path-to-image ,uri)
      :stream stream)))

(defun diagram ()
  (let ((input-string   (parameter "names"))
	(label-style    (parameter "label-style"))
	(add-top-bottom (parameter "add-top-bottom"))
	(key            (cond ((parameter "these") :these)
			      ((parameter "descendants") :descendants)
			      ((parameter "ancestors") :ancestors)
			      (T (error "Unknown key used in `diagram`"))))) 
    (destructuring-bind (input-names filename uri rel-path)
	(encoded-string key input-string label-style add-top-bottom)
      (progn
	(unless (probe-file
		 (concatenate 'string
			      *local-directory*
			      rel-path))
	  (graph input-names filename label-style))
	(push
	 (hunchentoot:create-static-file-dispatcher-and-handler
	  uri (local rel-path))
	 hunchentoot:*dispatch-table*)
	(make-diagram-page input-names uri)))))

(defun examples ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"examples.tmpl" '()
     :stream stream)))
  


;;; debugging

(defun debug-mode-on ()
  (setq *catch-errors-p* NIL)
  (setq *show-lisp-errors-p* T))

(defun debug-mode-off ()
  (setq *catch-errors-p* T)
  (setq *show-lisp-errors-p* NIL))
