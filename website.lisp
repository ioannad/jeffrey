(in-package :jeffrey.website)

;; ## Website module
;;
;; Server and initial dispatch table

(defvar *server*)

(defun local (rel-path)
  (concatenate 'string *local-directory* rel-path))

(load-nodes-html)

(setq hunchentoot:*dispatch-table*  ; dispatches folders, files, and pages
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
	     "/" 'generate-index-page)))

(defun start-website (&key (address "localhost") (port "8080"))
  (setq *server* (hunchentoot:start
		  (make-instance 'easy-acceptor
				 :address address
				 :port port))))

;; strangely, the above does not work for localhost on my side...
;; but the below does transmit the website fine at
;; http://127.0.0.1:8080/

(defun start-website??  (&key (port 8080))
  (setq *server* (hunchentoot:start
		  (make-instance 'easy-acceptor
				 :port port))))

(defun stop-website ()
  (stop *server*))

(setq html-template:*default-template-pathname*
      (concatenate 'string *local-directory* "www/"))

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

;; ## Diagrams pages

;; ### Encoding filenames
;;
;; I represent a subset of '(0 1 ... 430) as a 431-long
;; sequence of 0s and 1s: position i will be 1 if the form with
;; HR. number i is in the subset. This sequence can be seen as an
;; integer in base 2. The encoding is this integer in base 36.

(defun names->bits (names)
  (loop for i from 0 to 430
     if (member i names)
     sum (expt 2 i)))

(defun names->code (names)
  (write-to-string (names->bits names) :base 36))

;; ### Getting the variables for the templates and the lock

(defun names->filename (names label-style)
  (format nil "~a-~a" label-style (names->code names)))

(defun names->uri (names label-style)
  (format nil "/~a.png" (names->filename names label-style)))

(defun names->rel-path (names label-style)
  (format nil "diagrams~a" (names->uri names label-style)))

(defun names->temp (names label-style)
  (format nil "diagrams/~a.temp"
	  (names->filename names label-style)))

(defun print-names (names &key kind)
  (concatenate
   'string
   (case kind
     ("random"
      (format nil
	      "the following (pseudo) randomly chosen forms: "))
     ("standard"
      (format nil
	      "the forms with HR numbers: ")))
   (format nil "~{~a~^, ~}" names)))

;; ## Graphing
;;
;; To take care of concurrency, I create a temp file when graphing,
;; whose existence `web-graph` has to check before attempting
;; to graph.

(defun create-temp (names label-style)
  (with-open-file (out (local (names->temp names label-style))
		       :direction :output
		       :if-exists :error
		       :if-does-not-exist :create)
    (format out "")))

(defun delete-temp (names label-style)
  (delete-file (local (names->temp names label-style))))

(defun lock-graph (names label-style)
  (create-temp names label-style)
  (graph names (names->filename names label-style) label-style)
  (delete-temp names label-style))

(defun file-exists-p (rel-path)
  (probe-file (local rel-path)))

(defun web-graph (names label-style)
  (unless (file-exists-p (names->rel-path names label-style))
    (if #1=(file-exists-p (names->temp names label-style))
	(loop while #1#
	   do (sleep 1))
	(lock-graph names label-style))))

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
  (let ((names (loop for name being the hash-keys of jeffrey.graph:*graph*
		  for string-name = (format nil "~a" name)
		  when (parameter string-name)
		  collect name)))
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
