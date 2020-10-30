(in-package :cage433-lisp-poker)
(require :cage433-lisp-utils)

(defun new-pack (&optional (rng (make-random-state)))
  (shuffle-vector rng (aops:generate* 'integer #'identity 52 :position)
          ))

