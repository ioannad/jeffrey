(in-package :jeffrey.main)

(read-all-data)

(setup-jeff-matrix *graph*)

(defun graph (names-list filename)
  (draw names-list filename))