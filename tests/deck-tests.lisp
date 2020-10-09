(in-package :cage433-lisp-poker)

(defun shuffle-test-suite ()
  (info "deck"
        (spec "Can deal 52 cards"
          (let ((deck (new-deck)))
            (progn
              (dotimes (i 52)
                (funcall deck :next))
                t)
            ))))

