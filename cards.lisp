(in-package :cage433-lisp-poker)

(defmacro mvbind (&rest args)
  "Abbreviation for 'multiple-value-bind'"
  `(multiple-value-bind ,@args))

(defun fold-left (fn lst x0)
  "If lst is (l0 l1 ... ln) returns (fn ln (fn ...(fn l1 (fn l0 x0))...))"
  (if (null lst)
	  x0
	  (fold-left fn (cdr lst) (funcall fn (car lst) x0))))

(defmacro acond (&rest clauses)
  "Anaphoric cond, from Paul Graham's 'On Lisp'. The result of any succesful test is put into
a variable called 'it'"
  (if (null clauses)
	nil
	(let ((cl1 (car clauses))
		  (sym (gensym)))
	  `(let ((,sym ,(car cl1)))
		(if ,sym
			(let ((it ,sym)) (declare (ignorable it)) ,@(cdr cl1))
			(acond ,@(cdr clauses)))))))


(defparameter *HAND-TYPES* (vector :running-flush :four-of-a-kind :full-house :flush
								   :run :three-of-a-kind :two-pair :pair :high-card))
(defparameter *SUIT-NAMES* (vector "H" "C" "D" "S"))
(defparameter *RANKS* (list 0 1 2 3 4 5 6 7 8 9 10 11 12))
(defparameter *RANK-NAMES* (vector "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))
(defparameter *LONG-RANK-NAMES* (vector "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "jack" "queen" "king" "ace"))
(defparameter *PLURAL-LONG-RANK-NAMES* (vector "twos" "threes" "fours" "fives" "sixes" "sevens" "eights" "nines" "tens" "jacks" "queens" "kings" "aces"))
(defparameter *PACK*
  (let ((pack (make-array 52 :element-type 'integer)))
	(dotimes (n 52)
	  (setf (aref pack n) n))
	pack))
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

(defun shuffle-deck (cards random-state)
  (let ((no-of-cards (length cards)))
	(dotimes (i no-of-cards)
	  (let ((index (+ i (random (- no-of-cards i) random-state))))
		(unless (= i index)
		  (rotatef (svref cards i) (svref cards index))))))
	cards)
(defun make-dealer (&key (random-state *random-state*) (cards (copy-seq *PACK*)))
  (labels ((shuffle () (shuffle-deck cards random-state)))
	(let ((next-card-no 0))
	  (lambda (x)
		(if (eq :shuffle x)
			(progn (shuffle)
				   (setf next-card-no 0))
			(prog1 (coerce (subseq cards next-card-no (+ next-card-no x)) 'list)
			  (incf next-card-no x)))))))
(defun rank-index (card)
  (the fixnum (mod card 13)))

(defun suit-index (card)
  (the fixnum (floor (/ card 13))))

(defun card-name-to-rank (card)
  (position (subseq card 0 (1- (length card))) *RANK-NAMES* :test #'string-equal))
(defun card-name-to-suit-number (card)
  (position (subseq card (1- (length card))) *SUIT-NAMES* :test #'string-equal))
(defun card-name-to-number (card)
  "It is convenient, and fast, to manipulate cards as integers. This function converts a card represented in the format '10H', 
into <rank-index> + 13 * <suit-index>. This function is the inverse of 'card-number-to-name"
  (if (numberp card)
	  card
	  (let ((rank (card-name-to-rank card))
			(suit (card-name-to-suit-number card)))
		(+ (* 13 suit) rank))))



(defun card-number-to-name (card)
  "Converts a card, represented as an integer, into a string representation, such as '10H'. This is the inverse
of 'card-name-to-number"
  (format nil "~a~a" (aref *RANK-NAMES* (rank-index card)) (aref *SUIT-NAMES* (suit-index card))))

(defun empty-hand () (list (list 0 0 0 0)
						   (make-array 13 :element-type 'integer :initial-element 0)
						   0))


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
	  (list suited-rank-bitmaps rank-counts (apply #'logior suited-rank-bitmaps)))))

(defun suited-rank-bitmaps (hand)
  (first hand))
(defun rank-counts (hand)
  (second hand))

(defun net-rank-bitmap (hand)
  (third hand))

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
  (awhen (flush hand)
	(let ((flush-bitmap (cadr it)))
	  (awhen (run flush-bitmap)
		(list *running-flush* (cadr it))))))

(defun has-n-of-same-rank (hand n &optional rank-to-ignore)
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
	  (awhen (has-n-of-same-rank hand 2 trip-rank)
		(list *full-house* trip-rank it)))))

(defun get-kickers (hand n ranks-to-ignore)
  (top-bits (fold-left (lambda (rank bitmap) (logxor (ash 1 rank) bitmap))
					   ranks-to-ignore
					   (net-rank-bitmap hand))
			n))
(defun three-of-a-kind (hand)
  (awhen (has-n-of-same-rank hand 3)
	(list *three-of-a-kind* it)))

(defun two-pair (hand)
  (awhen (has-n-of-same-rank hand 2)
	(let ((top-pair-rank it))
	  (awhen (has-n-of-same-rank hand 2 top-pair-rank)
		(let* ((bottom-pair-rank it)
			   (kicker (get-kickers hand 1 (list top-pair-rank bottom-pair-rank))))
		  (list *two-pair* top-pair-rank bottom-pair-rank kicker))))))

(defun pair (hand)
  (awhen (has-n-of-same-rank hand 2)
	(let* ((top-pair-rank it)
		   (kickers (get-kickers hand 3 (list top-pair-rank))))
	  (list *pair* top-pair-rank kickers))))

(defun high-card (hand)
  (list *high-card* (get-kickers hand 5 nil)))

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

(defun map-n-times (fn N)
  (do ((i 0 (1+ i))
	   (result '() (cons (funcall fn i) result)))
	  ((= i N) (reverse result))))

(defun increment-hole-card-win-count (dealer N-players stats counts)
  (funcall dealer :shuffle)
  (let ((hole-cards (map-n-times (lambda (i) (declare (ignore i)) (funcall dealer 2)) N-players))
		(face-cards (funcall dealer 5)))
	(let* ((analyses (sort (mapcar (lambda (players-hole-cards)
									 (let ((hand (make-hand-from-cards (append players-hole-cards face-cards))))
									   (list
										(analyse-hole-cards players-hole-cards)
										(analyse-hand hand)
										hand)))
								   hole-cards)
						   #'hand> :key #'second))
		   (winning-analysis (second (car analyses))))
	  (dolist (a analyses)
		(incf (gethash (first a) counts 0)))
	  (let* ((winners (remove-if-not (lambda (analysis) (eq analysis winning-analysis)) analyses :key #'second))
			 (win-fraction (/ 1.0 (length winners))))
		(dolist (winners-hole-cards (mapcar #'first winners))
		  (incf (gethash winners-hole-cards stats 0.0)
				win-fraction))))))

(defun hole-card-win-stats (N-players N-games)
  (let ((stats (make-hash-table :test #'equal))
		(counts (make-hash-table :test #'equal))
		(dealer (make-dealer)))
	(dotimes (i N-games)
	  (increment-hole-card-win-count dealer N-players stats counts))
	(let ((results '()))
	  (maphash (lambda (hole-card-type sum-win-fractions) (push (cons hole-card-type (/ sum-win-fractions (gethash hole-card-type counts))) results))
			   stats)
	  (sort results #'> :key #'cdr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Build probability table ;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *test-name* nil)



(defun check-approximately-equals (value expected-value tolerance &key text (double-format "~,3f"))
  (or (< (abs (- value expected-value)) tolerance)
	  (progn
		(when *test-name*
		  (format t "Test failed: ~a~%" *test-name*))
		(when text
		  (format t "~a~%" text))
		(format t "Expected ~?, but got ~?~%" double-format (list expected-value) double-format (list value))
		nil)))

(defun check-within-%age-tolerance (value expected-value tolerance &key text (double-format "~,3f"))
  (check-approximately-equals value expected-value
							  (* tolerance (max (abs value) (abs expected-value)))
							  :text text :double-format double-format))

(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (unless result
;; 	  (format t "... passed: ~a~%" *test-name*)
	  (format t "... failed: ~a ~a~%" *test-name* form))
  result)



(defparameter *running-flush-king-high* (make-hand-from-cards (list "KH" "QH" "JH" "10H" "9H" "AC" "AD")))
(defparameter *running-flush-ace-high* (make-hand-from-cards (list "KH" "QH" "JH" "10H" "9H" "AH" "AD")))
(defparameter *running-flush-five-high* (make-hand-from-cards (list "5H" "4H" "3H" "2H" "AH" "AC" "AD")))
(defparameter *four-aces* (make-hand-from-cards (list "3S" "3C" "3D" "AH" "AC" "AS" "AD")))
(defparameter *four-threes* (make-hand-from-cards (list "3S" "3C" "3D" "3H" "AC" "AS" "AD")))
(defparameter *full-house-threes-on-tens* (make-hand-from-cards (list "3S" "3C" "3D"  "10C" "10S" "AD" "KD")))
(defparameter *full-house-tens-on-threes* (make-hand-from-cards (list "3S" "3C" "10H"  "10C" "10S" "AD" "KD")))
(defparameter *flush-Q-high-1* (make-hand-from-cards (list "QH" "JH" "8H" "7H" "6H" "10C" "9C")))
(defparameter *flush-Q-high-2* (make-hand-from-cards (list "QH" "JH" "9H" "7H" "6H" "10C" "9C")))
(defparameter *run-Q-high* (make-hand-from-cards (list "QH" "JH" "10C" "9H" "8H" "10D" "10S")))
(defparameter *run-10-high* (make-hand-from-cards (list  "10C" "9H" "8H" "7H" "6H" "10D" "10S")))
(defparameter *three-kings* (make-hand-from-cards (list "KH" "KC" "KS" "QS" "10S" "8D" "6D")))
(defparameter *three-queens* (make-hand-from-cards (list "AC" "QH" "QC" "QS" "10S" "8D" "6D")))
(defparameter *tens-and-eights* (make-hand-from-cards (list "10C" "10D" "8S" "8D" "4C" "KS" "2S")))
(defparameter *nines-and-eights* (make-hand-from-cards (list "9C" "9D" "8S" "8D" "AC" "KS" "2S")))
(defparameter *tens-and-threes* (make-hand-from-cards (list "10C" "10D" "3S" "3D" "AC" "KS" "2S")))
(defparameter *two-tens* (make-hand-from-cards (list "10C" "10D" "AC" "KS" "8H" "5H" "3D")))
(defparameter *two-nines-1* (make-hand-from-cards (list "9C" "9D" "AC" "KS" "8H" "5H" "3D")))
(defparameter *two-nines-2* (make-hand-from-cards (list "9C" "9D" "AC" "QS" "8H" "5H" "3D")))
(defparameter *king-high-1* (make-hand-from-cards (list "KC" "QC" "10D" "9D" "5S" "4S" "2H")))
(defparameter *king-high-2* (make-hand-from-cards (list "KC" "QC" "10D" "8D" "5S" "4S" "2H")))
(defparameter *queen-high* (make-hand-from-cards (list "QC" "10D" "8D" "6S" "5S" "4S" "2H")))

(defun card-names-to-bitmap (cards)
  (apply #'logior (mapcar (lambda (card) (ash 1 (card-name-to-rank card)))
						  cards)))

(check
  (equal (list *running-flush* 11) (analyse-hand *running-flush-king-high*))
  (equal (list *running-flush* 12) (analyse-hand *running-flush-ace-high*))
  (equal (list *running-flush* 3) (analyse-hand *running-flush-five-high*))
  (equal (list *four-of-a-kind* 12) (analyse-hand *four-aces*))
  (equal (list *four-of-a-kind* 1) (analyse-hand *four-threes*))
  (equal (list *full-house* 8 1) (analyse-hand *full-house-tens-on-threes*))
  (equal (list *full-house* 1 8) (analyse-hand *full-house-threes-on-tens*))
  (equal (list *flush* (card-names-to-bitmap (list "QH" "JH" "8H" "7H" "6H"))) (analyse-hand *flush-Q-high-1*))
  (equal (list *flush* (card-names-to-bitmap (list "QH" "JH" "9H" "7H" "6H"))) (analyse-hand *flush-Q-high-2*))
  (equal (list *run* 10) (analyse-hand *run-Q-high*))
  (equal (list *run* 8) (analyse-hand *run-10-high*))
  (equal (list *three-of-a-kind* 11) (analyse-hand *three-kings*))
  (equal (list *three-of-a-kind* 10) (analyse-hand *three-queens*))
  (equal (list *two-pair* 8 6 (card-names-to-bitmap (list "KS"))) (analyse-hand *tens-and-eights*))
  (equal (list *two-pair* 7 6 (card-names-to-bitmap (list "AC"))) (analyse-hand *nines-and-eights*))
  (equal (list *two-pair* 8 1 (card-names-to-bitmap (list "AC"))) (analyse-hand *tens-and-threes*))
  (equal (list *pair* 8 (card-names-to-bitmap (list "AC" "KS" "8H"))) (analyse-hand *two-tens*))
  (equal (list *pair* 7 (card-names-to-bitmap (list "AC" "KS" "8H"))) (analyse-hand *two-nines-1*))
  (equal (list *pair* 7 (card-names-to-bitmap (list "AC" "QS" "8H"))) (analyse-hand *two-nines-2*))
  (equal (list *high-card* (card-names-to-bitmap (list "KC" "QC" "10D" "9D" "5S"))) (analyse-hand *king-high-1*))
  (equal (list *high-card* (card-names-to-bitmap (list "KC" "QC" "10D" "8D" "5S"))) (analyse-hand *king-high-2*))
  (equal (list *high-card* (card-names-to-bitmap (list "QC" "10D" "8D" "6S" "5S"))) (analyse-hand *queen-high*))
  
  )





