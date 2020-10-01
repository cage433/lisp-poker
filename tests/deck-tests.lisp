(in-package :cage433-lisp-poker)

(defun shuffle-test-suite ()
  (info "shuffle"
        (spec "shuffled-deck has same length"
          (let ((deck (new-deck)))
            (progn
              (dotimes (i 52)
                (funcall deck :next))
                t)
            )))
  )

