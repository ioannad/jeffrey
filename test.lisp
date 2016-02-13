(in-package :jeffrey.test)

(defparameter *simple-test-data*
  '((:Form1 :a T)
    (:a :b t)
    (:b :d t)
    (:b :c t)
    (:d :c nil)
    (:c :b nil)
    (:e :b t)
    (:e :h t)
    (:d :g t)
    (:c :f t)
    (:Form0 :f nil)))

(defun test-read ()
  (set-difference *simple-test-data*
		  (graph-to-implications
		   (implications-to-graph *simple-test-data*))
		  :test :equal))

(defun test-add-edge ()  ;;Maybe add more basic tests like this one. 
  (handler-case (let ((node (make-node))
		      (edge (make-edge (make-node) t)))
		  (add-edge node edge)
		  (add-edge node edge)
		  node)
    (simple-error (e) e)
    (:no-error (v) (error "Should signal SIMPLE-ERROR ~a" v))))

(defun setup-test ()
  (setq *nodes*
	(add-top-bottom (implications-to-graph *simple-test-data*))))

(defun test-predicates ()
  (setup-test)
  (assert (not (implies-not-p (call :Form1) (call :a))))
  (assert (not (implies-not-p (call :b) (call :a))))
  (assert (implies-not-p (call :c) (call :b)))
  (assert (implies-not-p (call :c) (call :a)))
  (assert (implies-not-p (call :c) (call :Form1)))
  (assert (implies-not-p (call :d) (call :a)))
  (assert (implies-not-p (call :Form0) (call :b)))
  (assert (not (implies-not-p (call :Form0) (call :h))))
  (assert (implies-not-p (call :Form0) (call :e)))
  (assert (implies-not-p (call :Form0) (call :Form1)))
  (assert (not (implies-not-p (call :Form0) (call :g))))
  (assert (not (implies-not-p (call :f) (call :g))))
  (assert (implies-not-p (call :g) (call :c)))
  (assert (not (implies-not-p (call :e) (call :a))))
  (assert (implies-not-p (call :d) (call :b)))
  (assert (implies-not-p (call :Form0) (call :f))))  

(defun test-read ()
  (assert (null (set-difference *simple-test-data* 
				(graph-to-implications 
				 (implications-to-graph *simple-test-data*))
				:test 'equal)))
  (assert (null (set-difference (graph-to-implications 
				 (implications-to-graph *simple-test-data*))
				*simple-test-data*
				:test 'equal))))



(defun fill-missing-positions-using-new-predicates (graph matrix)
  "Fills in the {matrix} using the predicates matrix-implies-p and 
matrix-implies-not-p, and the information in the nodes of {graph}.
It returns the filled in matrix."
  (loop for node-i being the hash-values of graph
     using (hash-key node-name-i)
     for i = (node-name-to-number node-name-i)
     unless (equal i 423)       ;; just to be sure
     do (prog1 (format t "~%Row = ~S: Columns = " i)
	  (setf (aref matrix i i) 1)
	  (loop for node-j being the hash-values of graph
	     using (hash-key node-name-j)
	     for j = (node-name-to-number node-name-j)
	     unless (equal j 423)       ;; just to be sure again
	     do (prog1 (format t "~S   " j)
		  (if (aref matrix i j) ;; if m-i-j exists
		      nil               ;; then do nothing,
		      ;; otherwise ask matrix-implies-p and
		      ;; matrix-implies-not-p.
		      (cond ((implies-p
			      node-i node-j matrix graph)
			     T)
			    ((implies-not-p
			      node-i node-j matrix graph)
			     T)
			    ;; If neither predicate has an answer,
			    ;; then the answer is unknown (code 0). 
			    (T (setf (aref matrix i j) 0))))))))
  ;; Returns the filled in matrix.
  matrix)


(defun matrix-equivalent-to-book1-p (matrix)
  "Tests if {matrix} has the same dimensions as book1, checks if 
fields with codes 1-4 in {matrix} have the same codes in {book1}, 
if fields with code 0 in {matrix} have code 0 or 7 in {book1}, and
ignores fields that have codes 5 and 6 in {book1}."
  (let* ((book1 (make-book1))
	 (n     (1- (first (array-dimensions matrix))))) 
    (format t "The new matrix has dimensions ~a.~%" (array-dimensions matrix))
    (if (equal (array-dimensions matrix) (array-dimensions book1))
	(format t "Matrices have the same dimensions.~%")
	(format t "Matrices do NOT have the same dimensions!~%"))
    ;; Set all code 7 fields in book1 to 0.
    (loop for i from 0 to n
       do (loop for j from 0 to n
	     do (if (equal (aref book1 i j) 7)
		    (setf (aref book1 i j) 0)
		    nil)))
    ;; Returns T if every item in the list of answers to the question whether
    ;; the two matrices have the same code, for each field, is T, unless
    ;; the codes are 5 or 6.
    (eval (cons 'and
		(loop for i from 0 to n
		   unless (equal i 423)
		   return (loop for j from 0 to n
			      collect (if (not (or (equal (aref book1 i j) 5)
						   (equal (aref book1 i j) 6)))
					  (if (equal (aref book1 i j) (aref matrix i j))
					      T
					      (prog2 (format t "In row ~a, column ~a, book1 has code ~a while matrix has code ~a."
							     i j (aref book1 i j) (aref matrix i j))
						  nil))
					  T)))))))

(defun print-comparison-file (book1 matrix)
  "Creates the text file {comparison-to-book1.txt} with a 
field-to-field comparison of the matrices {book1} and {matrix}.
assumes that the matrices are square and have the same dimensions."
  (let ((n (1- (first (array-dimensions book1)))))
    (with-open-file (stream "comparison-to-book1.txt" 
			    :direction :output 
			    :if-exists :supersede)
      (format stream "*Field to field comparison of the matrix book1 from Howard and Rubin's \"Consequences of the Axiom of Choice Project\".* ~%~% Each field is given as a pair, whose first coordinate is the value in book1 and second coordinate the value in the predicate-induced matrix. Each field is marked by \"m-i-j =\", which indicates that that field is in row i and column j, starting the counting from 0. Equivalently, this is field that codes whether form with number i implies the form with number j.~%~%")
      (loop for i from 0 to n
	 do (progn (format stream "Row ~a:   " i )
		   (loop for j from 0 to n
		      do (format stream
				 "m-~a-~a = (~a, ~a)   "
				 i j (aref book1 i j) (aref matrix i j))))))))


(defun test-predicates-with-book1 (create-file?)
  "Temporarily creates a matrix with {book1} and a matrix induced by 
the new predicates, and uses the predicate {matrix-equal-to-book1-p} 
to check if these two matrices are equivalent. If run with {NIL} as 
an argument, this is all it does. If run with {T} as an argument (or 
anything non {NIL}), it also produces a file with a side-to-side 
comparison of each field in the two matrices."
  (let* ((book1  (make-book1))
	 (graph  (make-graph-from-book1 book1))
	 (matrix (fill-missing-positions-using-new-predicates 
		   graph
		   (make-minimal-matrix-from-graph graph))))
    (if (matrix-equivalent-to-book1-p matrix)
	(format t "Matrices equivalent.")
	;; The predicate will throw an error anyway if they are not
	;; equivalent. 
	(format t "Matrices NOT equivalent."))
    (if create-file?
	(prog2 (print-comparison-file book1 matrix)
	    (format t "Comparison file created."))
	nil)))
	  
