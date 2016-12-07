(defsystem jeffrey
  :components ((:file "packages")
	       (:file "graph" :depends-on ("packages"))
	       (:file "parse" :depends-on ("packages"))
	       (:file "process-strings" :depends-on ("packages"))
	       (:file "read" :depends-on ("packages"
					  "graph"
					  "parse"
					  "process-strings"))
	       (:file "predicates" :depends-on ("packages"
						"graph"
						"read"))
	       (:file "draw" :depends-on ("packages"
					  "graph"
					  "read"
					  "predicates"))
	       (:file "labelmaker" :depends-on ("packages"
						"graph"
						"read"))
	       (:file "main" :depends-on ("graph"
					  "read"
					  "predicates"
					  "draw"))
	       (:file "website" :depends-on ("parse" "main"))
	       (:file "example-form-groups" :depends-on ("main"))
	       (:file "test" :depends-on ("packages"
					  "graph"
					  "read"
					  "predicates")))
  :depends-on ("maxpc" "split-sequence"
	       "external-program" "parenscript"
	       "hunchentoot" "cl-who"))
