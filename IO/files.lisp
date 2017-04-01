;; # IO/Files package
;;
;; All related to reading and writing files goes here


(defun make-filename  (postfix-ending input-filename)
  (concatenate 'string
	       *local-directory*
	       "diagrams/"
	       input-filename
	       postfix-ending))
