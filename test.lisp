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
  (assert (implies-not-p (call :Form0) (call :f)))
)  