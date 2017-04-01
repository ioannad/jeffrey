(in-package :jeffrey.website)

;; ## Website module
;;
;; Server and initial dispatch table

(defvar *server*)

(defun local (rel-path &optional (project-name "ac-consequences"))
  (concatenate 'string
	       *local-directory* "data/" project-name "/" rel-path))

(defun setup-dispatch (project-name) ; For cgraph use "ac-consequences"
  (load-nodes-html)
  
  (setq hunchentoot:*dispatch-table*  ; dispatches folders, files, and pages
	(list (hunchentoot:create-folder-dispatcher-and-handler
	       "/examples/preview/" (local "examples/preview/"))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/examples/" (local "examples/"))
	      (hunchentoot:create-static-file-dispatcher-and-handler
	       "/random-header.js"
	       (local "assets/random-header.js"))
	      (hunchentoot:create-static-file-dispatcher-and-handler
	       "/jeffrey.css" (local "assets/jeffrey.css"))
	      (hunchentoot:create-prefix-dispatcher
	       "/diagram" 'diagram)
	      (hunchentoot:create-prefix-dispatcher
	       "/random-diagram" 'random-diagram)
	      (hunchentoot:create-prefix-dispatcher
	       "/selection-diagram" 'selection-diagram)
	      (hunchentoot:create-prefix-dispatcher
	       "/examples" 'examples)
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/math-snippets/" (local "math-snippets/"))
	      (hunchentoot:create-prefix-dispatcher
	       "/names-and-statements" 'names-and-statements)
	      (hunchentoot:create-prefix-dispatcher
	       "/selected-diagram" 'selected-diagram)
	      (hunchentoot:create-prefix-dispatcher
	       "/" 'generate-index-page))))
  
(defun start-website (&key (address "localhost") (port "8080")
		      &optional (project-name "ac-consequences"))
  (setup-dispatch project-name)
  (setq *server* (hunchentoot:start
		  (make-instance 'easy-acceptor
				 :address address
				 :port port))))

(defun stop-website ()
  (stop *server*))

(setq html-template:*default-template-pathname*
      (concatenate 'string *local-directory* "web/templates/"))

;; ## Index page

(defun generate-index-page ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"jeffrey.tmpl"
     (list :title "Choiceless Grapher"
	   :index? T
	   :error? NIL
	   :bad-forms (format nil "~{~a~^, ~}" *bad-forms*))
     :stream stream)))

;; ## Examples page
    
(defun examples ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"examples.tmpl" '()
     :stream stream)))

;; ## names-and-statements page

(defun names-and-statements ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"forms-statements.tmpl" '()
     :stream stream)))

;; ## search function for names-and-statements
(defun get-names ()
  (loop for name being the hash-keys of jeffrey.graph:*graph*
     for string-name = (format nil "~a" name)
     when (parameter string-name)
     collect name))

(defun keywords->names (keywords)
  (remove-duplicates
   (loop for keyword in keywords
      append (keyword->names keyword)))) ;; find-names with tries?
	  
(defun filter-keywords ())
;  (let* ((keywords   (input->keywords (parameter "keywords")))
;	 (show-names (append (get-names)
;			     (keywords->names keywords))))
   
    
    

;; ## Diagrams pages


;; ### Filling the diagram templates and getting the pages online.

(defmacro get-graph (names label-style)
  `(push
    (hunchentoot:create-static-file-dispatcher-and-handler
     (names->uri ,names ,label-style)
     (local (names->rel-path ,names ,label-style)))
    hunchentoot:*dispatch-table*))

(defmacro fill-template (names label-style bad-input &key kind)
  `(with-output-to-string (stream)
     (html-template:fill-and-print-template
      #P"jeffrey.tmpl"
      (list :title "Implication diagram"
	    :index? NIL
	    :error? NIL
	    :print-names (print-names ,names :kind ,kind)
	    :bad-input ,bad-input
	    :path-to-image (names->uri ,names ,label-style))
      :stream stream)))

(defun diagram ()
  (destructuring-bind (names bad-input label-style) (process-input)
    (web-graph names label-style)
    (get-graph names label-style)
    (fill-template names label-style bad-input :kind "standard")))

(defun random-diagram ()
  (let* ((n (parse-integer (parameter "n")))
	 (names (sort (random-HR-numbers n) #'<)))
    (web-graph names "fancy")
    (get-graph names "fancy")
    (fill-template names "fancy" nil :kind "random")))

(defun selection-diagram ()
  (let ((names (get-names)))
    (web-graph names "fancy")
    (get-graph names "fancy")
    (fill-template names "fancy" nil :kind "standard")))


;;; debugging

(defun debug-mode-on ()
  (setq *catch-errors-p* NIL)
  (setq *show-lisp-errors-p* T))

(defun debug-mode-off ()
  (setq *catch-errors-p* T)
  (setq *show-lisp-errors-p* NIL))
