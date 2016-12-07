(in-package :jeffrey.website)

#|
Mainly following:

* Adam Tornhill's www.adamtornhill.com/articles/lispweb.htm 

* Matthew Snyder's msnsnyder.info/posts/2011/07/lisp-for-the-web-part-ii 

* Marc Kuo's kuomarc.wordpress.com/2012/05/13/12-steps-to-build-and-deploy-common-lisp-in-the-cloud-and-comparing-rails/
|#

(defvar *server*)

(defmacro static (uri rel-filepath)
  `(hunchentoot:create-static-file-dispatcher-and-handler
    ,uri
    (concatenate 'string *local-directory* ,rel-filepath)))

(setq hunchentoot:*dispatch-table*
      (list (static "/logo.jpg"    "www/logo.jpg")
	    (static "/jeffrey.css" "www/jeffrey.css")))

(defun start-website ()
  (setq *server* (start (make-instance 'easy-acceptor
				       :port 8080))))

(defun stop-website ()
  (stop *server*))

(defmacro standard-page ((&key title) &body body)
  `(with-html-output (*standard-output* nil
					:prologue t
					:indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en"
	    :lang "en"
	    (:head
	     (:meta :http-equiv "Content-Type"
		    :content "text/html;charset=utf-8")
	     (:title ,title)
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "/jeffrey.css"))
	    (:body
	     (:p (:img :src "/logo.jpg"
		       :alt "Choiceless Grapher"
		       :class "logo"
		       :width "450"))
	     ,@body))))

(defmacro define-url-fun ((name) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher
	    ,(format nil "/~(~a~).htm" name) ',name)
	   *dispatch-table*)))

(define-url-fun (choiceless-grapher)
    (standard-page
     (:title "Choiceless Grapher")
     (:h1 "Choiceless Grapher")
     (:h2 "Diagram creator for consequences of the axiom of choice (AC).")
     (:p "Write content later...")
     (:form :action "/requesting-diagram.htm" :method "post"
	    (:p "Please input space separated Howard Rubin numbers as integers:"
		(:br)
		(:input :type :text
			:name "input-names"
		       :class "txt"))
	    (:p (:input :type "submit"
			:value "Request diagram"
			:class "btn")))))

(define-url-fun (requesting-diagram)
  (let ((input-string (parameter "input-names")))
    (let ((input-names (parse input-string (=names))))
      (unless (or (null input-string)
		  (zerop (length input-string)))
	(progn (graph input-names "diagram" "fancy" "png")
	       (push (static "/diagram.png" "diagrams/diagram.png")
		     *dispatch-table*)
	       (redirect "/diagram.htm"))))))

(define-url-fun (diagram)
  (standard-page
      (:title "Implication diagram")
    (:h2 "The diagram of implications you requested:")
    (:p "A boldfaced arrow means strict implication, i.e., there is a model of ZF set theory in which the destination form is true and the origin form is false. For the moment, please look up the references to these implications at the Consequences of the Axiom of Choice Project (LINK TBA). If the image is too small, right click and select 'Show image' (or similar), or resize your browser window.")
    (:img :src "/diagram.png"
	  :alt "Your diagram")))
  

#| 

(defun controller-css ()
  (setf (hunchentoot:content-type* *reply*) "text/css")
  (css-lite:css
   (("body")
    (:width "70%" :margin "0 auto" :font-family "verdana"))
   ((:h2)
    (:font-size "18px" :font-style "bold"))
   (("#header")
    (:padding "8px"))
   (("#header .logo")
    (:display "block" :margin "0 auto"))
   ((p)
    (:font-family "verdana" :font-size "18px"))))



|#
