(in-package :jeffrey.main)

(read-all-data)

(setup-jeff-matrix *graph*)

(defvar *names* (loop for key being the hash-keys of *graph*
		   collect key))

(defun name-transformer (key names)
  (let ((transformer (case key
                       (:these       (lambda (node)
                                       (list node)))
                       (:descendants #'descendants)
                       (:ancestors   #'ancestors)
                       (:interval    #'interval)))
        ;; the transformers require nodes input so we get the nodes
        (nodes (loop for name in names collect (gethash name *graph*))))
    (remove-duplicates
     (mapcar
      #'node-name ; the transformers return nodes, so we get the names
      (append nodes ; add the input nodes to the graph
              (if (eq key :interval)
                  ;; interval takes exactly the two given nodes as input
                  (apply transformer nodes)
                  ;; ancestors or descendants of all the NAMES will be included
                  (reduce 'append (mapcar transformer nodes))))))))

(defun graph (names-list filename
              &optional style ending
              &aux (filename% filename))
  (let ((pathname (pathname filename)))
    (when (null style)
      (setq style "numbers"))
    (when (null (or ending
                    (pathname-type pathname)))
      (setq ending "png"))
    (when (null ending)
      (setq ending (pathname-type pathname))
      (setq filename% (pathname-name pathname)))
    (when (pathname-directory pathname)
      (warn "The parent directory of your output file is ignored.
Output files will be placed in ..jeffrey/diagrams/"))
    (draw names-list filename% style ending)))

(defun graph-descendants (names-list filename &optional style ending)
  (graph (name-transformer :descendants names-list)
         filename style ending))

(defun graph-ancestors (names-list filename &optional style ending)
  (graph (name-transformer :ancestors names-list)
         filename style ending))

(defun graph-interval (name-1 name-2 filename &optional style ending)
  (graph (name-transformer :interval (list name-1 name-2))
         filename style ending))

;;; Random graph functions

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

(assert (= 10 (length (remove-duplicates
		       (random-numbers
			10 '(0 1 2 3 4 5 6 7 8 9 10 11) '())))))

(defun random-HR-numbers (n)
  (append '(0 1)
	  (random-numbers n
			  (node-names *graph*)
			  (append *bad-forms*
				  '(0 1)))))

(defun random-graph (amount-of-nodes filename &optional style ending)
  (graph (random-HR-numbers amount-of-nodes) filename style ending))
