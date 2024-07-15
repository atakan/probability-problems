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

