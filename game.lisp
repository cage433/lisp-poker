(in-package :cage433-lisp-poker)

(defstruct player
  (name (error "name") :type symbol)
  (strategy)
  )


(defparameter *BET-TYPES* (list :call :raise :fold :check))

(defstruct bet
  (bet-type (error "bet-type") :type symbol)
  (amount (error "amount") :type float))


(defstruct game-state 
  (players (error "players") :type (vector player))
  (i-dealer (error "dealer") :type integer)
  (small-blind (error "small-blind") :type float)
  (blind (error "blind") :type float)
  (next-to-bet (error "next to bet") :type integer)
  (pot-total (error "pot total") :type float)
  (bets (error "bet") :type (vector bet))
  (player-bet-totals (error "player-bet-totals") :type (vector float))
  (players-remaining (error "players-remaining") :type (vector boolean))
  (hole-cards (error "hole-cards") :type (vector (vector integer)))
  (turn (error "turn") :type (vector integer))
  (river (error "river") :type (vector integer))
  )


;(defun initialise-game (players i-dealer deck small-blind blind)
  ;(let* (
         ;(n-players (length players))
        ;(i-blind (mod (+ 1 i-dealer) n-players))
        ;(i-small-blind (mod (+ 2 i-dealer) n-players))
        ;(hole-cards 



(defun random-strategy (seed)
  (let ((rng (sb-ext:seed-random-state seed)))
    (lambda (i-player game-state)
      (progn
        (if (aref (game-state-players-remaining game-state) i-player)
          (let ((minimum-bet (calc-minimum-bet game-state i-player))
                (x (random 1.0 rng)))
            (cond
              ((and (> minimum-bet 0) (> x 0.66)) (make-bet :bet-type :raise :amount (* 2.0 minimum-bet)))
              ((and (> minimum-bet 0) (> x 0.33))  (make-bet :bet-type :call :amount minimum-bet))
              ((> minimum-bet 0)  (make-bet :bet-type :fold :amount 0.0))
              (t (make-bet :bet-type :check :amount 0.0)))))))))

(defun calc-minimum-bet (game-state i-player)
  (declare (ignorable game-state i-player))
  (error "Implement calc-minimum-bet"))

(defun make-players (n-players)
  (make-array (list n-players) 
              :initial-contents (map-n-times 
                                  (lambda (i) (make-player :name (make-symbol (format nil "~A" i)) :strategy (random-strategy (random 1234))))
                                  n-players)
              :element-type 'player)
    )

