#|
# Projects package

Handling of different projects.
|#

(defvar *projects* (list (1 "ac-consequences")
			 (2 "cardinal-characteristics")))

(defvar *current-project* "ac-consequences")

(defun create-project (project-name); UNDER CONSTRUCTION
  )

(defun add-project (project-name)
  (if (member project-name *projects* :test 'second)
      (error "Project name is in use.")
      (create-project project-name)))

