(in-package :cage433-lisp-poker)


(defparameter *HAND-TYPES* (vector :running-flush :four-of-a-kind :full-house :flush
								   :run :three-of-a-kind :two-pair :pair :high-card))
(defparameter *SUIT-NAMES* (vector "H" "C" "D" "S"))
(defparameter *RANKS* (list 0 1 2 3 4 5 6 7 8 9 10 11 12))
(defparameter *RANK-NAMES* (vector "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))
(defparameter *LONG-RANK-NAMES* (vector "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "jack" "queen" "king" "ace"))
(defparameter *PLURAL-LONG-RANK-NAMES* (vector "twos" "threes" "fours" "fives" "sixes" "sevens" "eights" "nines" "tens" "jacks" "queens" "kings" "aces"))

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

(defun map-n-times (fn N)
  (do ((i 0 (1+ i))
	   (result '() (cons (funcall fn i) result)))
	  ((= i N) (reverse result))))




