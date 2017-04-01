#| 
# name-sets package

Contains functions, which manipulate sets of names, or
return sets of names (standard, example, and random sets).
|#


;; ## Set manipulation
;; Descendants, ancestors, and possible-paths.

(defun name-transformer (key names)
  (let ((funct (case key
		 (:these       (lambda (node)
				 (list node)))
		 (:descendants #'descendants)
		 (:ancestors   #'ancestors))))
    (remove-duplicates
     (append
      names
      (loop for name in names
	 append (map 'list 
		     #'node-name
		     (funcall funct (gethash name *graph*))))))))
  
(assert (equal '(0 1 2 3 4 5)
	       (name-transformer :these '(0 1 2 3 4 5))))
(assert (equal '(0 18 64 80 127 300 301 389 390)
	       (sort (name-transformer :descendants '(80 301 64))
		     #'<)))
(assert (equal '(1 188 193 255 256 258 261 262)
	       (sort (name-transformer :ancestors '(255 188))
		     #'<)))

(defun all-paths (A B)
  "Returns a list of all names involved in implications from forms
A to B"
  (let ((A-desc (descendants A))
	(B-anc  (ancestors   B)))
    (union (list A B)
	   (intersection A-desc B-anc))))

;; ## Pseudo random sets

(defun node-names (graph)                    ;; Should this be somewhere else?
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

(assert (= 10 (length (remove-duplicates
		       (random-numbers
			10 '(0 1 2 3 4 5 6 7 8 9 10 11) '())))))

(defun random-HR-numbers (n)
  (append '(0 1)
	  (random-numbers n
			  (node-names *graph*)
			  (append *bad-forms*
				  '(0 1)))))




 
