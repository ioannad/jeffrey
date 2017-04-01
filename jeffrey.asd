(defsystem jeffrey
    :components ((:file "packages")
		 (:file "core/graph" :depends-on ("packages"))
		 (:file "parse/parse" :depends-on ("packages"))
		 (:file "parse/process-strings" :depends-on ("packages"))
		 (:file "core/read" :depends-on ("packages"
						 "core/graph"
						 "parse/parse"
						 "parse/process-strings"))
		 (:file "core/predicates" :depends-on ("packages"
						       "core/graph"
						       "core/read"))
		 (:file "core/draw" :depends-on ("packages"
						 "core/graph"
						 "core/read"
						 "core/predicates"))
		 (:file "util/labelmaker" :depends-on ("packages"
						       "core/graph"
						       "core/read"))
		 (:file "main" :depends-on ("core/graph"
					    "core/read"
					    "core/predicates"
					    "core/draw"))
		 (:file "parse/parse-web-input" :depends-on ("packages"))
		 (:file "parse/latex-in-html"   :depends-on ("packages"
							     "core/graph"))
		 (:file "web/web-draw" :depends-on ("packages"))
		 (:file "web/website" :depends-on ("packages"
						   "main"
						   "web/web-draw"
						   "parse/parse-web-input"
						   "parse/latex-in-html"))
		 (:file "util/example-form-groups" :depends-on ("main"))
		 (:file "test/test" :depends-on ("packages"
						 "core/graph"
						 "core/read"
						 "core/predicates")))
    :depends-on ("maxpc" "split-sequence"
			 "external-program"
			 "hunchentoot" "html-template"))
