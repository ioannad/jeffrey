(in-package :jeffrey.main)

(defvar *welcome*  ; ascii art made with `figlet cgraph`.
  "~%
~%
  ___ __ _ _ __ __ _ _ __ | |__  ~%
 / __/ _` | '__/ _` | '_ \| '_ \ ~%
| (_| (_| | | | (_| | |_) | | | |~%
 \___\__, |_|  \__,_| .__/|_| |_|~%
     |___/          |_|          ~%
~%
Welcome to the cgraph, the interactive implication diagram maker.~%
~%")

(defvar *first-options*
  "~%
Press ENTER⏎ to load 'Consequences of the Axiom of Choice' project (default data).~%
Press ESC    to load a different project or to start a new project.~%
~%")

(defvar *menu-text*
  "Menu~%----~%
Press 1 to draw a diagram from a list of form numbers.~%
Press 2 to see a list of the form numbers and their statements.~%
Press 3 to show the current project name.~%
Press 4 to change project.~%
Press 5 to show examples (requires web-browser).~%
Press 6 to run a website locally.~%
Press 7 for more information.")

(defun process-project-input (input)
  (if (equal input "N")
      (format *standard-output* "This option will be available in the next commit. Check github.com/ioannad/jeffrey.")
      (let ((project-number (handle-non-integer-input input choose-project)))
	(if #1=(member *projects* :test 'first)
	    (setf *current-project* (second #1#))
	    (progn (format *standard-output* "Project number not recognised.~%")
		   (choose-project))))))

(defun choose-project ()
  (format *standard-output*
	  "Available databases:~%~{~{~a. ~a~}~%~}"
	  *projects*)
  (format *standard-output*
	  "Enter the number of the project you wish to change to, or enter N to begin a new project.~%")
  (let ((input (read *standard-input* nil nil)))
    (process-project-input input)))
    
(defun change-project ()
  (when (file-exists-p *projects-file*)
    (setf *projects* (with-open-file (in *projects-file*
					 :direction :input)
		       (with-standard-io-syntax (read in)))))
  (choose-project))

(defun choose-project ()
  (format *standard-output* *first-options*)
  (let ((action (read-char)))
    (if (equal action #\escape)
	(change-project)
	(unless (equal action #\return)
	  (first-action)))))

(defun load-data ()
  (read-data *current-project*)
  (setup-jeff-matrix *graph*)
  (setf *names* (loop for key being the hash-keys of *graph*
		   collect key)))

;; ## The main program

(defun main ()
  (format *standard-output* *welcome*)
  (choose-project)
  (load-data))

