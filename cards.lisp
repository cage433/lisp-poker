(defparameter *HAND-TYPES* (vector :running-flush :four-of-a-kind :full-house :flush
								   :run :three-of-a-kind :two-pair :pair :high-card))
(defparameter *SUIT-NAMES* (vector "H" "C" "D" "S"))
(defparameter *RANK-NAMES* (vector "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))
(defparameter *LONG-RANK-NAMES* (vector "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "jack" "queen" "king" "ace"))
(defparameter *PLURAL-LONG-RANK-NAMES* (vector "twos" "threes" "fours" "fives" "sixes" "sevens" "eights" "nines" "tens" "jacks" "queens" "kings" "aces"))
(defparameter *PACK*
  (let ((pack (make-array 52 :element-type 'integer)))
	(dotimes (n 52)
	  (setf (aref pack n) n))
	pack))
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
  (mod card 13))

(defun suit-index (card)
  (floor (/ card 13)))

(defun card-name-to-number (card)
  "It is convenient, and fast, to manipulate cards as integers. This function converts a card represented in the format '10H', 
into <rank-index> + 13 * <suit-index>. This function is the inverse of 'card-number-to-name"
  (if (numberp card)
	  card
	  (let ((rank (position (subseq card 0 (1- (length card))) *RANK-NAMES* :test #'string-equal))
			(suit (position (subseq card (1- (length card))) *SUIT-NAMES* :test #'string-equal)))
		(+ (* 13 suit) rank))))



(defun card-number-to-name (card)
  "Converts a card, represented as an integer, into a string representation, such as '10H'. This is the inverse
of 'card-name-to-number"
  (format nil "~a~a" (aref *RANK-NAMES* (rank-index card)) (aref *SUIT-NAMES* (suit-index card))))

(defun empty-hand () (list 0 0 0 0) )

(defun make-hand-from-cards (cards)
  (if (stringp (car cards))
    (make-hand-from-cards (mapcar #'card-name-to-number cards))
    (let ((hand (empty-hand)))
      (dolist (card cards)
        (setf (nth (suit-index card) hand) 
              (logior (ash 1 (rank-index card))
                      (nth (suit-index card) hand))))
      hand)))


(defun has-flush-p (hand)
    (find-if (lambda (x) (>= x 5)) hand :key #'logcount))
