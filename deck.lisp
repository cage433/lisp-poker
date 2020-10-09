(in-package :cage433-lisp-poker)

(defun new-pack ()
  (let ((pack (make-array 52 :element-type 'integer)))
    (dotimes (n 52)
      (setf (aref pack n) n))
    pack))

(defun shuffle-deck (rng cards)
  (let ((no-of-cards (length cards)))
    (dotimes (i no-of-cards)
      (let ((index (random-state:random-int rng i (1- no-of-cards))))
        (unless (= i index)
          (rotatef (svref cards i) (svref cards index))))))
  cards)


(defun new-deck (&key (cards (new-pack)))
  (let ((pack cards)
        (i-next-card 0))
    (lambda (command &optional arg)
      (ecase command
        (:shuffle (shuffle-deck arg pack))
        (:next 
         (let ((card (svref cards i-next-card)))
           (progn (incf i-next-card)
                  card))
         )))))
