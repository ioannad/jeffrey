(defsystem jeffrey
  :components ((:file "packages")
	       (:file "graph" :depends-on ("packages"))
	       (:file "read" :depends-on ("packages" "graph"))
	       (:file "predicates" :depends-on ("packages" "graph" "read"))
	       (:file "test" :depends-on ("packages" "graph" "read" "predicates"))
	       ))
