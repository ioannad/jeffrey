(in-package :jeffrey.main)

(read-all-data)

(setup-jeff-matrix *graph*)

(defun graph (names-list filename style &optional (ending "png"))
  (draw names-list filename style ending))

(defun node-names (graph)
  (loop for name being the hash-keys of graph
     collect name))

;;; Random graph functions

(defun random-number (list except-these)
  (let ((l (length #1=(set-difference list except-these))))
    (nth (random l) #1#)))

(defun random-numbers (n list except-these)    
  (loop for i from 1 to n
     collecting (random-number list (append except-these numbers))
     into numbers
     finally (return numbers)))
	    
(defun random-graph (amount-of-nodes filename style &optional (ending "png"))
  (graph 
   (append (random-numbers amount-of-nodes 
			   (node-names *graph*)
			   (append *bad-forms*
				   '(0 1)))
	   '(1 0))
   filename 
   style
   ending))

;;; graph descendants

(defun descendants-names (name-list) ;=> list of node-names
  "Returns a list of the names of the descendants of the nodes with
names in `name-list`."
  (loop for name in name-list
     append (map 'list
		 #'node-name
		 (descendants (gethash name *graph*)))))

(defun graph-descendants (name-list filename style
			  &optional (ending "pdf"))
  "Graphs the descendants of the nodes with names in `names-list`."
  (graph (append (descendants-names name-list) name-list)
	 filename
	 style
	 ending))
    
