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
                              )
                  ))
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
 
