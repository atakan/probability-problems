(defparameter *init-cond* '(3 2 2))

(defvar *changes* '((1/6) (-1 1 0)
		    (1/6) (-1 0 1)))

(defun make-zeros-list (n)
  (loop for i from 1 to n collect 0))

(defun changes-tower-prob (n)
  "create all possibilities with one +1, one -1 and rest 0s. Turn it into a list with probabilities and return.
   this is essentially all permutations of (1 -1 0 ... 0).
   we can do this by starting n-2 0s and inserting 1 and -1 in all possible slots."
  (let ((my-zeroes (make-zeros-list n))
	(zeros-with-ones nil))
    (dotimes (i n zeros-with-ones)
      ;;; instead of the following, I should check if the current element is 0 and on ly then subst and push. At least for the next step.
      (push (substitute 1 0 my-zeroes :start i :count 1) zeros-with-ones))))
    
    
  
