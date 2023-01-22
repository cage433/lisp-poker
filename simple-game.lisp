(defun 0-to (n) (map-n-times #'identity n))

(defun play-one-round-games (blind rules N-games)
  (let* ((N-players (length rules))
		 (stacks (make-array N-players :element-type 'double-float :initial-element 0d0))
		 (sum-bets 0.0))
	(labels ((move-to-pot (i-player amt)
			   (decf (aref stacks i-player) amt)
			   (incf sum-bets amt)))
	  (dotimes (i-game N-games)
		(let ((highest-card -1.0)
			  (i-winner)
			  (num-bets 0))
		  (mapc (lambda (i-player rule)
				  (move-to-pot i-player blind)
				  (let ((card (random 1.0)))
					(when (> card (aref rule num-bets))
					  (incf num-bets)
					  (move-to-pot i-player 1.0)
					  (when (> card highest-card)
						(setq highest-card card)
						(setq i-winner i-player)))))
				(0-to N-players) rules)
		  (move-to-pot (or i-winner (1- N-players)) (- sum-bets)))))
	(map 'vector (lambda (w) (/ w N-games)) stacks)))

(defun initialise-rules (limit N-players)
  (mapcar (lambda (i-player) (make-array (1+ i-player) :initial-element limit :element-type 'double-float))
		  (0-to N-players)))

(time (play-one-round-games 1.0 (make-list 10 :initial-element 0.9) 100000))

(defun play-simple-game (lhs-blind lhs-limit rhs-blind rhs-limit)
  (let ((card1 (random 1.0))
		(bet 1.0))
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

(defun play-n-times (game-fn n)
  (labels ((divide-by (lst a) (mapcar (lambda (x) (/ x a)) lst))
		   (averages (lst) (divide-by lst n))
		   (square (lst) (mapcar #'* lst lst)))
	(do* ((i 0 (1+ i))
		  (result (funcall game-fn) (funcall game-fn))
		  (sums result (mapcar #'+ sums result))
		  (sum-sqrs (square result) (mapcar #'+ sum-sqrs (square result))))
		 ((= i n) (let* ((means (averages sums))
						 (variances (mapcar #'- (averages sum-sqrs) (square means))))
					(values means (divide-by (mapcar #'sqrt variances) (sqrt n)))))
	  ())))








(defun golden-search-minimization (low high fn tol)
  (let ((mu 0.3897))
	(labels ((next-pt (x0 x1)
			   (+ x0 (* mu (- x1 x0))))
			 (recurse (x-s y-s)
			   (dbind ((x-near x-mid x-far) (y-near y-mid y-far) )
					  (list x-s y-s)
					  (if (< (abs (- x-near x-far)) tol)
						  (list x-mid (second y-s))
						  (let* ((x-next (next-pt x-mid x-far))
								 (y-next (funcall fn x-next)))
							(if (> y-next y-mid)
								(recurse (list x-next x-mid x-near) (list y-next y-mid y-near))
								(recurse (list x-mid x-next x-far) (list y-mid y-next y-far)))
							)))))
	  (let* ((x-s (list low (next-pt low high) high))
			 (y-s (mapcar fn x-s)))
		(recurse x-s y-s)))))


(defun optimise-betting-limits-for-simple-game ()
  (let ((player1-blind 1.0)
		(player2-blind 1.0))
	(labels ((make-game-fn (player1-limit player2-limit)
			   (lambda () (play-simple-game player1-blind player1-limit player2-blind player2-limit))))
	  )
	))






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

(let ((mu 0.38197))
  (defun golden-search-minimization (fn low high tol)
	(labels ((next-x (vals)
			   (dbind (x0 x1 x2) (mapcar #'car vals)
					  (+ (* x1 (- 1 mu))
						 (* mu (if (< (- x1 x0) (- x2 x1)) x2 x1)))))
			 (drop (vals)
			   (if (< (cdr (first vals))
					  (cdr (last vals)))
				   (butlast vals)
				   (cdr vals)))
			 (improve (vals)
			   )
			 ))))
