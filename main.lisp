(in-package :jeffrey.main)

(read-all-data)

(setup-jeff-matrix *graph*)

(defun graph (names-list filename style)
  (draw names-list filename style))

(defun node-names (graph)
  (loop for name being the hash-keys of graph
     collect name))

(defun random-number (list except-these)
  (let ((l (length #1=(set-difference list except-these))))
    (nth (random l) #1#)))

(defun random-numbers (n list except-these)    
  (loop for i from 1 to n
     collecting (random-number list (append except-these numbers))
     into numbers
     finally (return numbers)))
	    
(defun random-graph (amount-of-nodes filename style)
  (graph 
   (append (random-numbers amount-of-nodes 
			   (node-names *graph*)
			   (append *bad-forms*
				   '(0 1)))
	   '(1 0))
   filename 
   style))
