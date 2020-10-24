(in-package :cage433-lisp-poker)
(require :cage433-lisp-utils)

(defun new-pack (&optional (rng (random-state:make-generator :mersenne-twister-64)))
  (coerce (shuffle-vector rng (aops:generate* 'integer #'identity 52 :position))
          'list))

