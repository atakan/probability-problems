;; The "Curious Coin Flipping Problem"
;; The idea is very simple: Create all possibilities, from this, get probabilities for each outcome (P(no coins change hands), P(1st player wins) etc.), then recusively (and with caching) get the length from each starting position, such as (1, 1, 1) or (1, 1, 4) or (2, 2, 2) etc.

(defun all-outcomes (n)
  "a function that returns all possibilities for a number of players.
   this is not very practical. a much better routine would give all possibilities one after the other, so it would not be limited by memory."
  (let ((outcomes '(())))
	(dotimes (i n outcomes)
	  (setf outcomes (all-outcomes-next outcomes)))))
      
		
(defun all-outcomes-next (ini-list)
  "given the outcomes in a step, gives the outcomes in the next step"
  (let ((result nil))
    (dolist (a ini-list result)
      (push (cons 'h a) result)
      (push (cons 't a) result))))

(defun zeros-like (a)
  "given a list or array, return an array consisting of zeros of the same length"
  (let ((l (length a)))
    (make-array l :initial-element 0)))

(defun outcome-probabilities (np)
  "given the number of players, gives probabilities for each outcome.
   right now, this is done by hand; it is a place holder, it is of course possible to calculate this, by using the routines above.
   in fact it has to be a lot more complicated than this, since for np>=5, more than 1 person may acquire coins and less than np-1 people may lose coins. So, maybe something like this? ( (1/4 (0 0 0)) (1/4 (2 -1 -1)) (1/4 (-1 2 -1)) (1/4 (-1 -1 2)). Yeah this looks good."
  
