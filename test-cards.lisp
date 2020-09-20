(in-package :cage433-lisp-poker)

(defparameter *running-flush-king-high* (make-hand-from-cards (list "KH" "QH" "JH" "10H" "9H" "AC" "AD")))
(defun hand-analysis-suite ()
  (labels ((check-hand (cards expected-analysis)
            (let ((analysis (analyse-hand (make-hand-from-cards cards))))
              (equal analysis expected-analysis))))
    (info "Hand analysis"
      (spec "King high running flush"
            (check-hand (list "KH" "QH" "JH" "10H" "9H" "AC" "AD")
                        (list *running-flush* 11)))
      (spec "Ace high running flush"
            (check-hand (list "KH" "QH" "JH" "10H" "9H" "AH" "AD")
                        (list *running-flush* 12)))
      (spec "Five high running flush"
            (check-hand (list "5H" "4H" "3H" "2H" "AH" "AC" "AD")
                        (list *running-flush* 3)))
      (spec "Four aces"
            (check-hand (list "3S" "3C" "3D" "AH" "AC" "AS" "AD")
                        (list *four-of-a-kind* 12)))
      (spec "Four threes"
            (check-hand (list "3S" "3C" "3D" "3H" "AC" "AS" "AD")
                        (list *four-of-a-kind* 1)))
      (spec "Full house 10s on 3s"
            (check-hand (list "3S" "3C" "10H" "10C" "10S" "AD" "KD") 
                        (list *full-house* 8 1)))
      (spec "Full house 3s on 10s"
            (check-hand (list "3S" "3C" "3D"  "10C" "10S" "AD" "KD") 
                        (list *full-house* 1 8)))
      )))
 
