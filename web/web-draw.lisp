;; # web-graph
;;
;; This package takes care of encoding all subsets of nodes
;; in unique short web-browser-acceptable strings, and takes
;; care of concurrency issues when two clients request the
;; same diagram simultaneously. 
;;
;; ## Encoding filenames
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

;; # concurrency
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

;; ## exported function

(defun web-graph (names label-style)
  (unless (file-exists-p (names->rel-path names label-style))
    (if #1=(file-exists-p (names->temp names label-style))
	(loop while #1#
	   do (sleep 1))
	(lock-graph names label-style))))
