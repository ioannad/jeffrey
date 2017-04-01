(in-package :jeffrey.parse-web-input)

(defun input-parse (input-string) ;=> input-names
    (mapcar #'parse-integer
	    (split-sequence #\Space input-string
			    :remove-empty-subseqs T)))

(defun get-bad-names (input-names)
  (loop for name in input-names
     if (or (member name *bad-forms*)
	    (> name 430)
	    (< name 0))
     collect name))

(defun get-good-names (input-names)
  (let ((bad-names (get-bad-names input-names)))
    (set-difference input-names bad-names)))

(defun pass-options-to-names (good-names key add-top-bottom)
  (remove-duplicates
   (if (and add-top-bottom (equal key :these))
       (append '(0 1) good-names)
       (name-transformer key good-names))))

(defun input->names (input-string key add-top-bottom)
  (if #1=(get-good-names (input-parse input-string))
      (sort (pass-options-to-names #1# key add-top-bottom)
	    #'<)
      '(1))) ; If all input forms are bad, fine, just show AC.

(defun input->keywords (string)
  (split-sequence:split-sequence #\Space string
				 :test 'equal
				 :remove-empty-subseqs T))

(defmacro process-input ()
  `(let ((input-string (parameter "names"))
	 (label-style  (parameter "label-style"))
	 (add-top-bot  (parameter "add-top-bottom"))
	 (key          (cond ((parameter "these")
			      :these)
			     ((parameter "descendants")
			     :descendants)
			     ((parameter "ancestors")
			      :ancestors))))
     (list (input->names input-string key add-top-bot)
	   (get-bad-names (input-parse input-string))
	   label-style)))


				     
