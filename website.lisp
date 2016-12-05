(in-package :jeffrey.website)

;(setf *web-server* (start-server :port 8080))


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
		    :href "/jeff.css"))
	    (:body
	     (:div :id "header"
		   (:img :src "/logo.svg"
			 :alt "Choiceless Grapher"
			 :class "logo")
		   (:span :class "strapline"
			  "Choiceless Grapher"))
	     ,@body))))

(defmacro define-url-fun ((name) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher
	    ,(format nil "/~(~a~).htm" name) ',name)
	   *dispatch-table*)))

