;; MC simulation for prob11 in Nahin's "Digital Dice"
;;
;; There are n-senators senators, n-against of them are against a certain bill. The bill will fail to pass if the opposition exceeds those in favor. n-missing random senators miss the vote. What is the probability that the vote for the bill will fail?
;;
;; steps:
;; create senate sample (1 for yea, 0 for nay)
;; remove missing senators by randomly choosing elements and moving to the end of sample
;; count remaining votes, excluding those at the end
;; do this again and again and find probability to pass

;; USAGE: e.g., (format t "~f" (missing-senators-problem 100 49 5 (round 1e7))

(defun swap-senators (lst el1 el2)
  (let ((tmp (elt lst el1)))
    (setf (elt lst el1) (elt lst el2))
    (setf (elt lst el2) tmp)))

(defun missing-senator-simulation (n-yea n-nay n-missing)
  "given the number of yea-sayers, nay-sayers and missing senators, this routine simulates the outcome of one voting process."
  (let ((senators (concatenate 'vector
			       (make-array n-yea :initial-element 1)
			       (make-array n-nay :initial-element 0))))
    (dotimes (i n-missing)
      (let* ((current-length (- (length senators) i))
	     (r-ele (random current-length))) ; choose a random element
	(swap-senators senators r-ele (1- current-length)))) ; swap it with the last element
    (let ((n-votes (- (length senators) n-missing)))
      (> (count 0 senators :end n-votes) (/ n-votes 2)))))

(defun missing-senators-problem (n-senators n-against n-missing n-simulation)
  (let ((n-fail 0))
    (dotimes (i n-simulation)
      (when (missing-senator-simulation (- n-senators n-against) n-against n-missing)
	(incf n-fail)))
    (/ n-fail n-simulation)))

(defun missing-senators-problem-take2 (n-senators n-against n-missing n-simulation)
  "in this alternative approach, we create a sample once; shuffle it repeatedly and check the first ... votes for the result. It may be faster, it may be slower; we need to check."
  )
