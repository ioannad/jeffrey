(in-package :jeffrey.test)

;; Disclaimer: The test data below has no connection with the real data
;; whatsoever. 
(defparameter *simple-test-matrix*
  (let ((matrix (make-array '(431 431) :initial-element NIL)))
    (flet ((set-code (name-1 name-2 code)
	     (if code
		 (setf (aref matrix name-1 name-2) 1)
		 (setf (aref matrix name-1 name-2) 3))))
      (set-code 1 2 t)
      (set-code 1 1 t)
      (set-code 2 3 t)
      (set-code 3 6 t)
      (set-code 3 7 t)
      (set-code 6 7 nil)
      (set-code 7 3 nil)
      (set-code 4 3 t)
      (set-code 4 5 t)
      (set-code 6 8 t)
      (set-code 7 9 t)
      (set-code 0 9 nil))
    matrix))

(defun setup-test-graph ()
  (format t "Initializing *graph* only with node-names...~%")
  (setf *graph* (make-hash-table))
  (loop for i from 0 to 430
     unless (member i *bad-forms*)
     do (setf (gethash i *graph*) (make-node i "" ""))))
  
(defun test-add-edge ()  ;;Maybe add more basic tests like this one. 
  (handler-case (let ((node (make-node 1 "" ""))
		      (edge (make-edge (make-node 2 "" "") t)))
		  (add-edge node edge)
		  (add-edge node edge)
		  node)
    (simple-error (e) (format t "add-edge works ok, does signal error: ~S~%" e))
    (:no-error (v) (error "Should signal SIMPLE-ERROR ~a" v))))

(defun setup-test ()
  (setup-test-graph)
  (format t "Saving test-matrix in *graph*.~%")
  (setf *graph* (add-top-bottom (matrix-to-graph *simple-test-matrix* *graph*)))
  (format t "*graph* completed with top and bottom nodes.~%"))

(defun test-predicates ()
  (setup-test)
  (assert (not (implies-not-p (call 1) (call 2))))
  (assert (not (implies-not-p (call 3) (call 2))))
  (assert (implies-not-p (call 7) (call 3)))
  (assert (implies-not-p (call 7) (call 2)))
  (assert (implies-not-p (call 7) (call 1)))
  (assert (implies-not-p (call 6) (call 2)))
  (assert (implies-not-p (call 0) (call 3)))
  (assert (not (implies-not-p (call 0) (call 5))))
  (assert (implies-not-p (call 0) (call 4)))
  (assert (implies-not-p (call 0) (call 1)))
  (assert (not (implies-not-p (call 0) (call 8))))
  (assert (not (implies-not-p (call 9) (call 8))))
  (assert (implies-not-p (call 8) (call 7)))
  (assert (not (implies-not-p (call 4) (call 2))))
  (assert (implies-not-p (call 6) (call 3)))
  (assert (implies-not-p (call 0) (call 9)))
  (format t "Passed test-predicates: predicates implies-p and implies-not-p appear correct.~%"))  

(defun test-read ()
  (setup-test-graph)
  (format t "Creating the processed-test-matrix from *simple-test-matrix*...~%")
  (let ((processed-test-matrix (graph-to-matrix
				(matrix-to-graph *simple-test-matrix*
						 *graph*))))
    (format t "Checking if matrices are equivalent...~%")
    (loop for i from 0 to 9
       unless (member i *bad-forms*)
       do (loop for j from 0 to 9
	     unless (or (member j *bad-forms*)
			(equal i j))
	     do (assert (equal (aref *simple-test-matrix*  i j)
			       (aref processed-test-matrix i j))))))
    (format t "Passed test-read: graph-to-matrix and matrix-to-graph appear to be inverse.~%"))

;;; -----------COMPARISON OF JEFFREY'S PREDICATES AGAINST BOOK1:

(defun fill-missing-positions-using-predicates (graph)
  "Fills in `*jeff-matrix*` using the predicates `implies-p` and 
`implies-not-p`, and the information in the nodes of `graph`."
  (format t "Asking all predicate questions.~%")
  (loop for node-i being the hash-values of graph
     using (hash-key name-i)
     do (format t "~%Row = ~S: " name-i)
       (loop for node-j being the hash-values of graph
	  using (hash-key name-j)
	  unless #1= (aref *jeff-matrix* name-i name-j)
	  do (format t ".")
	    (cond ((implies-p     node-i node-j)  T)
		  ((implies-not-p node-i node-j)  T)
		  (T (setf #1# 0))))
       (format t "~%"))
  (format t "Filled *jeff-matrix* with all the answers of implies-p and implies-not-p.~%"))

(defun test-matrix-equivalency (matrix book1-matrix)
  "Tests if fields with codes 1-4 in `matrix` have the same codes in
`book1-matrix`, if fields with code 0 in `matrix` have code 0 or 7 in
`book1-matrix`, and ignores fields that have codes 5 and 6 in 
`book1-matrix`."
  (let ((ok-pairs
	 '((1 1) (2 2) (3 3) (4 4) (0 0) (0 7) (0 5) (0 6))))
    (loop for i from 0 to 430
       unless (member i *bad-forms*)
       do (loop for j from 0 to 430
	     for jeff-code = (aref matrix i j)
	     for book-code = (aref book1-matrix i j)
	     unless (member j *bad-forms*)
	     do (assert (member #1=(list jeff-code book-code)
				ok-pairs
				:test #'equal)
			()
			"Wrong code in place (~a,~a) => ~a"
			i j #1#))))
  (format t "Passed test-matrix-equivalency.~%"))

(defun test-all ()
  ;; test data
  (format t "Setting up test...~%")
  (setup-test)
  (format t "Testing add-edge...~%")
  (test-add-edge)
  (format t "Testing predicates with the test-matrix...~%")
  (test-predicates)
  (format t "Testing the reading module...~%")
  (test-read)
  ;; book1
  (format t "Testing equivalence of the filled out *jeff-matrix* with book1...~%")
  (let ((book1-matrix (read-all-data)))
    (setup-jeff-matrix *graph*)
    (fill-missing-positions-using-predicates *graph*)
    (test-matrix-equivalency *jeff-matrix* book1-matrix)))
