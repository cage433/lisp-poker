(in-package :cage433-lisp-poker)

(defparameter *RUNS-RANK-BITMAPS*
  (vector
   (+ 4096 2048 1024 512 256)			;; 10 -> A
   (+ 2048 1024 512 256 128)			;; 9 -> K
   (+ 1024 512 256 128 64)				;; 8 -> Q
   (+ 512 256 128 64 32)				;; 7 -> J
   (+ 256 128 64 32 16)					;; 6 -> 10
   (+ 128 64 32 16 8)					;; 5 -> 9
   (+ 64 32 16 8 4)						;; 4 -> 8
   (+ 32 16 8 4 2)						;; 3 -> 7
   (+ 16 8 4 2 1)						;; 2 -> 6
   (+ 8 4 2 1 4096))			        ;; A -> 5
  "list of the bitmaps corresponding to all possible runs"
  )

(defparameter *high-card* 0)
(defparameter *pair* 1)
(defparameter *two-pair* 2)
(defparameter *three-of-a-kind* 3)
(defparameter *run* 4)
(defparameter *flush* 5)
(defparameter *full-house* 6)
(defparameter *four-of-a-kind* 7)
(defparameter *running-flush* 8)

(defparameter *hand-names* (list
							"high card" "pair" "two pair"
							"three of a kind" "run" "flush"
							"full house" "four of a kind" "running flush"))

(defun make-hand-from-cards (cards)
  (if (stringp (car cards))
    (make-hand-from-cards (mapcar #'card-name-to-number cards))
    (let ((suited-rank-bitmaps (make-list 4 :initial-element 0))
		  (rank-counts (make-array 13 :element-type 'integer :initial-element 0)))
      (dolist (card cards)
        (setf (nth (suit-index card) suited-rank-bitmaps) 
              (logior (ash 1 (rank-index card))
                      (nth (suit-index card) suited-rank-bitmaps)))
		(incf (aref rank-counts (rank-index card))))
	  (list suited-rank-bitmaps rank-counts (apply #'logior suited-rank-bitmaps) cards))))

(defun suited-rank-bitmaps (hand)
  (first hand))
(defun rank-counts (hand)
  (second hand))

(defun net-rank-bitmap (hand)
  (third hand))

(defun hand-cards (hand)
  (fourth hand))
(defun top-bits (bitmap n)
  (labels ((rec (bitmap acc)
			 (if (<= (logcount bitmap) n)
				 (ash bitmap acc)
				 (rec (ash bitmap -1) (1+ acc)))))
	(rec bitmap 0)))

(defun flush (hand)
  (awhen (find-if (lambda (suit-bitmap)
					(declare (type (unsigned-byte 16) suit-bitmap))
						  (>= (logcount suit-bitmap) 5))
				  (the cons (suited-rank-bitmaps hand)))
	(list *flush* (top-bits it 5))))

(defun run (hand-or-bitmap)
  "If a run exists returns"
  (if (listp hand-or-bitmap)
      (run (net-rank-bitmap hand-or-bitmap))
      (awhen (position-if (lambda (run-bitmap) (= run-bitmap (logand run-bitmap hand-or-bitmap)))
                          *RUNS-RANK-BITMAPS*)
        (list *run* (- 12 it)))))

(defun running-flush (hand)
  (let ((suit-bitmaps (suited-rank-bitmaps hand)))
    (awhen (position-if 
             (lambda (suit-bitmap) (>= (logcount suit-bitmap) 5))
             suit-bitmaps)
      (let* ((matching-cards (remove-if-not (lambda (card) (equalp (suit-index card) it)) (hand-cards hand) )))
        (awhen  (run (make-hand-from-cards matching-cards))
          (list *running-flush* (cadr it)))))))
   

(defun has-n-of-same-rank (hand n &key (rank-to-ignore nil))
  "Returns the highest rank for which there exist at least 'n' cards."
  (find-if (lambda (rank) (and (not (eq rank rank-to-ignore))
							   (>= (aref (rank-counts hand) rank) n)))
			   *RANKS*
			   :from-end t))

(defun four-of-a-kind (hand)
  (awhen (has-n-of-same-rank hand 4)
	(list *four-of-a-kind* it)))

(defun full-house (hand)
  (awhen (has-n-of-same-rank hand 3)
	(let ((trip-rank it))
	  (awhen (has-n-of-same-rank hand 2 :rank-to-ignore trip-rank)
		(list *full-house* trip-rank it)))))

(defun get-kickers (hand n &key (ranks-to-ignore nil))
  (top-bits (foldl (lambda (bitmap rank) (logxor (ash 1 rank) bitmap))
					   (net-rank-bitmap hand)
					   ranks-to-ignore
					   )
			n))

(defun three-of-a-kind (hand)
  (awhen (has-n-of-same-rank hand 3)
	(list *three-of-a-kind* it)))

(defun two-pair (hand)
  (awhen (has-n-of-same-rank hand 2)
	(let ((top-pair-rank it))
	  (awhen (has-n-of-same-rank hand 2 :rank-to-ignore top-pair-rank)
		(let* ((bottom-pair-rank it)
			   (kicker (get-kickers hand 1 :ranks-to-ignore (list top-pair-rank bottom-pair-rank))))
		  (list *two-pair* top-pair-rank bottom-pair-rank kicker))))))

(defun pair (hand)
  (awhen (has-n-of-same-rank hand 2)
	(let* ((top-pair-rank it)
		   (kickers (get-kickers hand 3 :ranks-to-ignore (list top-pair-rank))))
	  (list *pair* top-pair-rank kickers))))

(defun high-card (hand)
  (list *high-card* (get-kickers hand 5)))

(defun analyse-hand (hand)
  (or (running-flush hand)
      (four-of-a-kind hand)
      (full-house hand)
      (flush hand)
      (run hand)
      (three-of-a-kind hand)
      (two-pair hand)
      (pair hand)
      (high-card hand)))

(defun hand> (list-1 list-2)
  (cond ((null list-1) nil)
		((> (car list-1) (car list-2)) t)
		((= (car list-1) (car list-2)) (hand> (cdr list-1) (cdr list-2)))))

(defun hand= (list-1 list-2)
  (equal list-1 list-2))

(defun analyse-hole-cards (hole-cards)
  (if (stringp (car hole-cards))
	  (analyse-hole-cards (mapcar #'card-name-to-number hole-cards))
	  (dbind (c1 c2) hole-cards
			 (let ((r1 (rank-index c1))
				   (r2 (rank-index c2)))
			   (cons
				(if (= (suit-index c1) (suit-index c2))
					:suited
					:unsuited)
				(sort (list r1 r2) #'<))))))

(defun empty-hand () (list (list 0 0 0 0)
						   (make-array 13 :element-type 'integer :initial-element 0)
						   0))
