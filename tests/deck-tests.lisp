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
    ;(format t "~A ~A ~A~%" observed-p err std-err)
    (< err 3.0)
    ))

(defun hand-frequency-test (name expected-hand-type n-runs expected-freq)
  (random-spec name
               (let ((pack (new-pack)))
                 (lambda (random-state)
                   (let* ((pred (lambda  ()
                                  (progn 
                                    (shuffle-vector random-state pack)
                                    (let ((cards (make-hand-from-cards (coerce (subseq pack 0 7) 'list))))
                                      (eq expected-hand-type (car (analyse-hand cards))))
                                    ))))
                     (test-frequency pred n-runs expected-freq)

                     )
                   ))
               )
  )
(defun hand-frequency-suite ()
  "Probabilities taken from 
    https://en.wikipedia.org/wiki/Poker_probability#Frequency_of_7-card_poker_hands "
  (let ((n-runs (* 10 1000)))
    (info "hand frequencies"
          (hand-frequency-test "running flush" *running-flush* n-runs 0.000311)
          (hand-frequency-test "four of a kind" *four-of-a-kind* n-runs 0.00168)
          (hand-frequency-test "full house" *full-house* n-runs 0.026)
          (hand-frequency-test "flush" *flush* n-runs 0.0303)
          (hand-frequency-test "straight" *run* n-runs 0.0462)
          (hand-frequency-test "three of a kind" *three-of-a-kind* n-runs 0.0483)
          (hand-frequency-test "two pair" *two-pair* n-runs 0.235)
          (hand-frequency-test "pair" *pair* n-runs 0.438)
          (hand-frequency-test "high card" *high-card* n-runs 0.174)
          )))

