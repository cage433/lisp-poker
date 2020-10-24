(in-package :cage433-lisp-poker)

(defun shuffle-test-suite ()
  (info "deck"
        (spec "Can deal 52 cards"
          (let ((deck (new-pack)))
            (eq 52 (length deck))
            ))))

(defun count-frequency (predicate-fn n-runs)
  (let ((acc 0))
    (dotimes (i n-runs)
      (if (funcall predicate-fn)
          (incf acc)))
    acc
    )
  )

(defun test-frequency (pred n p)
  (let* ((variance (* p (- 1 p)))
         (std-err (sqrt (/ variance n)))
         (n-success (count-frequency pred n))
         (observed-p (/ n-success n 1.0))
         (err (abs (/ (- observed-p p) std-err))))
    (format t "~A ~A ~A~%" observed-p err std-err)
    (< err 3.0)
    ))

(defun hand-frequency-suite ()
  (info "hand frequencies"
        (random-spec "four of a kind"
                     (lambda (rng)
                       (let ((pred (lambda  ()
                                  (let ((cards (make-hand-from-cards (subseq (new-pack rng) 0 7))))
                                    (four-of-a-kind cards)
                                    ))))
                         (test-frequency pred 1000000 0.00168)
                         )
                       )
              )))

