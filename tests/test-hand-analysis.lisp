(in-package :cage433-lisp-poker)

(defun hand-analysis-suite ()
  (labels ((check-hand (cards expected-analysis)
             (let ((analysis (analyse-hand (make-hand-from-cards cards))))
               (if (not (equal analysis expected-analysis))
                   (format t "failed, expected ~A, got ~A~%" expected-analysis analysis))
               (equal analysis expected-analysis))))
    (info "Hand analysis"
          (info "Running flush"
                (spec "King high"
                  (check-hand (list "KH" "QH" "JH" "10H" "9H" "AC" "AD")
                              (list *running-flush* 11)))
                (spec "Ace high "
                  (check-hand (list "KH" "QH" "JH" "10H" "9H" "AH" "AD")
                              (list *running-flush* 12)))
                (spec "Five high running flush"
                  (check-hand (list "5H" "4H" "3H" "2H" "AH" "AC" "AD")
                              (list *running-flush* 3)))

                (spec "9 high with more of same suit"
                  (check-hand (list "5C" "6C" "7C" "8C" "9C" "AC" "7S")
                              (list *running-flush* 7)
                              ))
                (random-spec "random hands"
                             (lambda (rng) 
                               (let* ((cards (random-running-flush rng))
                                      (analysis (analyse-hand (make-hand-from-cards cards))))
                                 (equalp (car analysis) *running-flush*))
                               )
                             :num-runs 10))
          (info "four of a kind"  
                (spec "aces"
                      (check-hand (list "3S" "3C" "3D" "AH" "AC" "AS" "AD")
                                  (list *four-of-a-kind* 12)))
                (spec "threes"
                      (check-hand (list "3S" "3C" "3D" "3H" "AC" "AS" "AD")
                                  (list *four-of-a-kind* 1))))
          (info "full house"
                (spec "10s on 3s"
                      (check-hand (list "3S" "3C" "10H" "10C" "10S" "AD" "KD") 
                                  (list *full-house* 8 1)))
                (spec "3s on 10s"
                      (check-hand (list "3S" "3C" "3D"  "10C" "10S" "AD" "KD") 
                                  (list *full-house* 1 8))))
          (info "flush"
                (spec "Queen high 1"
                      (check-hand (list "QH" "JH" "8H" "7H" "6H" "10C" "9C") 
                                  (list *flush* (+ 1024 512 64 32 16))))
                (spec "Queen high 2"
                      (check-hand (list "QH" "JH" "9H" "7H" "6H" "10C" "9C") 
                                  (list *flush* (+ 1024 512 128 32 16))))) 
          (info "run" 
                (spec "queen high"
                      (check-hand (list "QH" "JH" "10C" "9H" "8H" "10D" "10S")
                                  (list *run* 10)))
                (spec "ten high"
                      (check-hand (list  "10C" "9H" "8H" "7H" "6H" "10D" "10S")
                                  (list *run* 8))))
          (info "three of a kind"
                (spec "kings"
                      (check-hand (list "KH" "KC" "KS" "QS" "10S" "8D" "6D")
                                  (list *three-of-a-kind* 11)))
                (spec "queens"
                      (check-hand (list "AC" "QH" "QC" "QS" "10S" "8D" "6D")
                                  (list *three-of-a-kind* 10))))
          (info "two pair" 
                (spec "10s & 8s"
                      (check-hand (list "10C" "10D" "8S" "8D" "4C" "KS" "2S")
                                  (list *two-pair* 8 6 2048)))
                (spec "9s & 8s"
                      (check-hand (list "9C" "9D" "8S" "8D" "AC" "KS" "2S")
                                  (list *two-pair* 7 6 4096)))
                (spec "10s & 3s"
                      (check-hand (list "10C" "10D" "3S" "3D" "AC" "KS" "2S")
                                  (list *two-pair* 8 1 4096))))
          (info "pair"
                (spec "two 10s"
                      (check-hand (list "10C" "10D" "AC" "KS" "8H" "5H" "3D")
                                  (list *pair* 8 (+ 4096 2048 64))))
                (spec "two 9s - 1"
                      (check-hand (list "9C" "9D" "AC" "KS" "8H" "5H" "3D")
                                  (list *pair* 7 (+ 4096 2048 64))))
                (spec "two 9s - 2"
                      (check-hand (list "9C" "9D" "AC" "QS" "8H" "5H" "3D")
                                  (list *pair* 7 (+ 4096 1024 64)))))
          (info "high card"
                (spec "King high - 1"
                      (check-hand (list "KC" "QC" "10D" "9D" "5S" "4S" "2H")
                                  (list *high-card* (+ 2048 1024 256 128 8))))
                (spec "King high - 2"
                      (check-hand (list "KC" "QC" "10D" "8D" "5S" "4S" "2H")
                                  (list *high-card* (+ 2048 1024 256 64 8))))))))

(defun count-frequency (predicate-fn n-runs)
  (let ((acc 0))
    (dotimes (i n-runs)
      (if (funcall predicate-fn)
          (incf acc)))
    acc))

(defun test-frequency (pred n p)
  (let* ((variance (* p (- 1 p)))
         (std-err (sqrt (/ variance n)))
         (n-success (count-frequency pred n))
         (observed-p (/ n-success n 1.0))
         (err (abs (/ (- observed-p p) std-err))))
      (< err 4.0)))

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
                     (test-frequency pred n-runs expected-freq))))))

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
 
