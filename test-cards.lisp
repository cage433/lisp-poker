(in-package :cage433-lisp-poker)

(defparameter *running-flush-king-high* (make-hand-from-cards (list "KH" "QH" "JH" "10H" "9H" "AC" "AD")))
(defun test-hand-analysis() 
  (info "Hand analysis"
    (spec "Should match 1"
			(equal (list *running-flush* 11) (analyse-hand *running-flush-king-high*)))
    (spec "Should match 2"
			(equal (list *running-flush* 11) (analyse-hand *running-flush-king-high*)))

		))
 
