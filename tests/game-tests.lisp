(in-package :cage433-lisp-poker)


(defun game-test-suite ()
  (info "game-structures"
    (spec "Can create player"
      (let ((player (make-player :name :fred :strategy nil)))
        (eq (player-name player) :fred)))
    (let ((players (make-players 10)))
      (info "players creation"
          (spec "length correct"
                (= 10 (length players)))
          (spec "name correct"
                (equal "0" (symbol-name (player-name (aref players 0)))))))
    (spec "Can create bet"
          (let ((bet (make-bet :bet-type :call :amount 100.0)))
            (and (= 100.0 (bet-amount bet)))))
        ))


