(in-package :cl-user)

(print ">>> Building system...")

(load (merge-pathnames "jeffrey.asd" *build-dir*))

(ql:quickload "jeffrey" :verbose t)

(pring ">>> Build complete.")
