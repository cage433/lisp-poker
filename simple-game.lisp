(defun play-simple-game (lhs-blind lhs-limit rhs-blind rhs-limit bet)
  (let ((card1 (random 1.0)))
	(if (< card1 lhs-limit)
		;; No bet - player 1 folds
		(list (- lhs-blind) lhs-blind)
		(let ((card2 (random 1.0)))
		  (if (< card2 rhs-limit)
			  ;; PLayer 1 bets - player 2 folds
			  (list rhs-blind (- rhs-blind))
			  ;; Both players bet
			  (if (> card1 card2)
				  ;; Player 1 wins
				  (list (+ rhs-blind bet) (- 0 rhs-blind bet))
				  ;; player 2 wins
				  (list (- 0 lhs-blind bet) (+ lhs-blind bet))))))))
(defun play-simple-game-n-times (blind bet lhs-limit rhs-limit n)
  (let ((sums (list 0 0))
		(sumsqrs (list 0 0)))
	(dotimes (i n)
	  (let ((result (play-simple-game blind lhs-limit blind rhs-limit bet)))
		(setq sums (mapcar #'+ sums result))
		(setq sumsqrs (mapcar #'+ sumsqrs (mapcar #'* result result)))))
	(let ((means (mapcar (lambda (s) (/ s n)) sums))
		  (meansqrs (mapcar (lambda (s) (/ s n)) sumsqrs)))
	  (let ((variances (mapcar #'- meansqrs (mapcar #'* means means))))
		(let ((stderrs (mapcar (lambda (v) (sqrt (/ v n))) variances)))
		  (list means stderrs))))))

(play-simple-game-n-times 1.0 1.0 0.1 0.9 100000)

(defun mean (sample)
  (/ (apply #'+ sample)
	 (length sample)))

(defun variance (sample)
  (- (mean (mapcar #'* sample sample))
	 (* (mean sample)
		(mean sample))))

(defun std-dev (sample)
  (sqrt (variance sample)))

(defun std-err (sample)
  (/ (std-dev sample) (sqrt (length sample))))

(defun value-random-fn (fn max-std-errs)
  (let ((valuer (function-valuer fn)))
	(funcall valuer 1000)
	(loop while (> (funcall valuer :std-err) max-std-errs)
		 do (funcall valuer 1000)
		 return (funcall valuer :mean))))

(defun function-valuer (fn)
  (let ((sum 0) (sum-sqrs 0) (count 0))
	(labels ((add-sample (n)
			   (let ((sample (funcall fn n)))
				 (incf sum (apply #'+ sample))
				 (incf sum-sqrs (apply #'+ (mapcar #'* sample sample)))))
			 (do-cmd (cmd)
			   (cond ((numberp cmd) (add-sample cmd))
					 ((eq :mean cmd) (/ sum count))
					 ((eq :variance cmd) (- (/ sum-sqrs count) (* (do-cmd :mean) (do-cmd :mean))))
					 ((eq :std-err cmd) (sqrt (/ (do-cmd :variance) count))))))
	  (lambda (cmd) (do-cmd cmd)))))

