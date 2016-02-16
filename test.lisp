(in-package :jeffrey.test)

;; Disclaimer: The test data below has no connection with the real data
;; whatsoever. 
(defparameter *simple-test-data*
  '((:Form1 :Form2 T)
    (:Form2 :Form3 t)
    (:Form3 :Form6 t)
    (:Form3 :Form7 t)
    (:Form6 :Form7 nil)
    (:Form7 :Form3 nil)
    (:Form4 :Form3 t)
    (:Form4 :Form5 t)
    (:Form6 :Form8 t)
    (:Form7 :Form9 t)
    (:Form0 :Form9 nil)))

(defun test-read ()
  (if (not (set-difference *simple-test-data*
			   (graph-to-implications
			    (implications-to-graph *simple-test-data*))
			   :test #'equal))
      (format nil "Functions graph-to-implications and implications-to-graph appear correct.")
      (error "Functions graph-to-implications and implications-to-graph are NOT correct.")))

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
  (setf *predicate-matrix* (make-minimal-matrix-from-graph *nodes*))
  (assert (not (implies-not-p (call :Form1) (call :Form2))))
  (assert (not (implies-not-p (call :Form3) (call :Form2))))
  (assert (implies-not-p (call :Form7) (call :Form3)))
  (assert (implies-not-p (call :Form7) (call :Form2)))
  (assert (implies-not-p (call :Form7) (call :Form1)))
  (assert (implies-not-p (call :Form6) (call :Form2)))
  (assert (implies-not-p (call :Form0) (call :Form3)))
  (assert (not (implies-not-p (call :Form0) (call :Form5))))
  (assert (implies-not-p (call :Form0) (call :Form4)))
  (assert (implies-not-p (call :Form0) (call :Form1)))
  (assert (not (implies-not-p (call :Form0) (call :Form8))))
  (assert (not (implies-not-p (call :Form9) (call :Form8))))
  (assert (implies-not-p (call :Form8) (call :Form7)))
  (assert (not (implies-not-p (call :Form4) (call :Form2))))
  (assert (implies-not-p (call :Form6) (call :Form3)))
  (assert (implies-not-p (call :Form0) (call :Form9)))
  (format nil "Predicates implies-p and implies-not-p appear correct."))  

(defun test-read ()
  (assert (null (set-difference *simple-test-data* 
				(graph-to-implications 
				 (implications-to-graph *simple-test-data*))
				:test 'equal)))
  (assert (null (set-difference (graph-to-implications 
				 (implications-to-graph *simple-test-data*))
				*simple-test-data*
				:test 'equal))))



(defun fill-missing-positions-using-new-predicates (graph)
  "Fills in the {*predicate-matrix*} using the predicates implies-p and 
implies-not-p, and the information in the nodes of {graph}."
  (loop for node-i being the hash-values of graph
     using (hash-key name-i)
     for i = (node-name-to-number name-i)
     unless (equal i 423)       ;; just to be sure
     do (prog1 (format t "~%Row = ~S: Columns = " i)
	  (setf (aref *predicate-matrix* i i) 1)
	  (loop for node-j being the hash-values of graph
	     using (hash-key name-j)
	     for j = (node-name-to-number name-j)
	     unless (equal j 423)       ;; just to be sure again
	     do (prog1 (format t "~S, " j)
		  (if #1= (aref *predicate-matrix* i j) ;; if m-i-j exists
		      nil                  ;; then do nothing,
		      ;; otherwise ask matrix-implies-p and
		      ;; matrix-implies-not-p, which will set the value
		      ;; of #1# to the correct value (2 or 4).
		      (cond ((implies-p     node-i node-j)  T)
			    ((implies-not-p node-i node-j)  T)
			    ;; If neither predicate has an answer,
			    ;; then the answer is unknown (code 0).
			    (T (setf #1# 0)))))))))


(defun matrix-equivalent-to-book1-p (matrix)
  "Tests if {matrix} has the same dimensions as book1, checks if 
fields with codes 1-4 in {matrix} have the same codes in {book1}, 
if fields with code 0 in {matrix} have code 0 or 7 in {book1}, and
ignores fields that have codes 5 and 6 in {book1}."
  (let* ((book1 (make-book1))
	 (n     (1- (array-dimension matrix 0)))) 
    (format t "The new matrix has dimensions ~a.~%" (array-dimensions matrix))
    (if (equal (array-dimensions matrix) (array-dimensions book1))
	(format t "Matrices have the same dimensions.~%")
	(format t "Matrices do NOT have the same dimensions!~%"))
    ;; Set all code 5,6, and 7 fields in book1 to 0.
    (loop for i from 0 to n
       do (loop for j from 0 to n
	     do (case #1= (aref book1 i j)
		      ((5 6 7) (setf #1# 0)))))
    ;; Returns T if every item in the list of answers to the
    ;; question whether the two matrices have the same code, for
    ;; each field, is T.
    (notany #'null
	    (loop for i from 0 to n
	       unless (equal i 423)
	       append
		 (loop for j from 0 to n
		    collect (let ((b-i-j (aref book1  i j))
				  (m-i-j (aref matrix i j)))
			      (if (equal b-i-j m-i-j)
				  T
				  (prog2
				      (format t "In row ~a, column ~a, book1 has code ~a while matrix has code ~a."
					      i j b-i-j m-i-j)
				      NIL))))))))
		    
(defun print-comparison-file (book1 matrix)
  "Creates the text file {comparison-to-book1.txt} with a 
field-to-field comparison of the matrices {book1} and {matrix}.
assumes that the matrices are square and have the same dimensions."
  (with-open-file (*standard-output* "comparison-to-book1.txt" 
				     :direction :output 
				     :if-exists :supersede)
    (format *standard-output*
	    "*Field to field comparison of the matrix book1 from Howard and Rubin's \"Consequences of the Axiom of Choice Project\".* ~%~% Each field is given as a pair, whose first coordinate is the value in book1 and second coordinate the value in the predicate-induced matrix. Each field is marked by \"m-i-j =\", which indicates that that field is in row i and column j, starting the counting from 0. Equivalently, this is field that codes whether form with number i implies the form with number j.~%~%")
      (loop for i from 0 to 430
	 do (progn (format *standard-output* "Row ~a:~%   " i )
		   (loop for j from 0 to 430
		      do (format *standard-output*
				 "m-~a-~a = (~a, ~a)~%   "
				 i j (aref book1 i j)
				 (aref matrix i j)))))))


(defun test-predicates-with-book1 (create-file?)
  "Temporarily creates a matrix with {book1} and a matrix induced
by the new predicates, and uses the predicate 
{matrix-equal-to-book1-p} to check if these two matrices are 
equivalent. If run with {NIL} as an argument, this is all it does.
If run with {T} as an argument (or anything non {NIL}), it also
produces a file with a side-to-side comparison of each field in the
two matrices."
  (let ((book1  (make-book1)))
    (set '*nodes* (make-graph-from-book1 book1))
    (set '*predicate-matrix* (make-minimal-matrix-from-graph *nodes*))
    (set '*predicate-matrix* (fill-missing-positions-using-new-predicates
			      *nodes*))
    (if (matrix-equivalent-to-book1-p *predicate-matrix*)
	(format t "Matrices equivalent.") 
	(format t "Matrices NOT equivalent."))
    (if create-file?
	(prog2 (print-comparison-file book1 *predicate-matrix*)
	    (format t "Comparison file created."))
	nil)))



(defun test-all ()
  ;; test data
  (setup-test)
  (test-predicates)
  (test-read)
  ;; book1 
  (test-predicates-with-book1 NIL))
