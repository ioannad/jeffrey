(in-package :jeffrey.main)

(read-all-data)

(setup-jeff-matrix *graph*)

(defun graph (names-list filename style)
  (draw names-list filename style))