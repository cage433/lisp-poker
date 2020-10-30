(in-package :cage433-lisp-poker)

(defun random-thing (rng things)
  (let ((n (random (1- (length things)) rng)))
    (if (vectorp things)
        (aref things n)
        (nth n things))
    ))

(defparameter *PACK*
  (let ((pack (make-array 52 :element-type 'integer)))
	(dotimes (n 52)
	  (setf (aref pack n) n))
	pack))

(defun choose-random-cards (rng n &key (cards-to-omit nil))
  (do ((next-card (random-thing rng *PACK*) (random-thing rng *PACK*))
       (acc nil (if (or (find next-card acc) (find next-card cards-to-omit))
                    acc
                    (cons next-card acc))))
      ((>= (length acc) n) acc)))

(defun random-running-flush (rng &key (top-rank nil) (suit_ nil))
  (let* ((top-rank (or top-rank (random-thing rng (nthcdr 4 *RANKS* ))))
         (suit (or suit_ (random-thing rng *SUIT-NAMES*)))
         (card-names 
           (mapcar (lambda (n) (format nil "~A~A" (aref *RANK-NAMES* n) suit)) (range (- top-rank 4) (+ top-rank 1))))
         (running-flush-cards (mapcar #'card-name-to-number card-names)))
    (append running-flush-cards (choose-random-cards rng 2 :cards-to-omit running-flush-cards))))

