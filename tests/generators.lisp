(in-package :cage433-lisp-poker)
(defun random-thing (rng things)
  (let ((n (random-state:random-int rng 0 (1- (length things)))))
    (if (vectorp things)
        (aref things n)
        (nth n things))
    )
  )
(defun range (from to)
  (do ((acc nil (cons i acc))
       (i from (incf i)))
      ((>= i to) (reverse acc))))

(defun generator-test-suite ()
  (info "Range"
        (spec "from 1 until 3"
              (equalp '(1 2) (range 1 3)))
        (spec "from 1 until 1"
              (equalp nil (range 1 1)))))

(defun choose-random-cards (rng n &key (cards-to-omit nil))
  (do ((next-card (random-thing rng *PACK*) (random-thing rng *PACK*))
       (acc nil (if (or (find next-card acc) (find next-card cards-to-omit))
                    acc
                    (cons next-card acc))))
      ((>= (length acc) n) acc)))

(defparameter *rng* (random-state:make-generator :mersenne-twister-64 12345))
(defun random-running-flush (rng &key (top-rank nil) (suit_ nil))
  (let* ((top-rank (or top-rank (random-thing rng (nthcdr 4 *RANKS* ))))
         (suit (or suit_ (random-thing rng *SUIT-NAMES*)))
         (card-names 
           (mapcar (lambda (n) (format nil "~A~A" (aref *RANK-NAMES* n) suit)) (range (- top-rank 4) (+ top-rank 1))))
         (running-flush-cards (mapcar #'card-name-to-number card-names)))

    (append running-flush-cards (choose-random-cards rng 2 :cards-to-omit running-flush-cards))
    ))
(format t "~A~%" (mapcar #'card-number-to-name  (random-running-flush *rng*))) 

(defun running-flush-generator-suite ()
  (info "random running flushes"
        (random-spec "Is a running flush"
                     (lambda (rng) 
                       (let* ((cards (random-running-flush rng))
                              (analysis (analyse-hand (make-hand-from-cards cards))))
                         (equalp (car analysis) *running-flush*))
                       )
                     :num-runs 100
                     )))

(defun running-flush-generator-test ()
  (
        random-spec "Is a running flush"
                     (lambda (rng) 
                       (let* ((cards (random-running-flush rng))
                              (analysis (analyse-hand (make-hand-from-cards cards))))
                         (progn
                           (format t "~A~%"
                           (mapcar #'card-number-to-name cards))
                           (format t "~A~%" analysis)
                         (equalp (car analysis) *running-flush*)))
                       )
                     :num-runs 100
                     ))
(cage433-lisp-utils::run-random-test (running-flush-generator-test)

                     :seed 5995244
           )

