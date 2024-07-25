(defparameter *init-cond* '(3 2 2))

(defvar *changes* '((1/6) (-1 1 0)
		    (1/6) (-1 0 1)))


#|
(defun changes-tower-prob (n)
  "create all possibilities with one +1, one -1 and rest 0s. Turn it into a list with probabilities and return.
   this is essentially all permutations of (1 -1 0 ... 0).
   we can do this by starting n-2 0s and inserting 1 and -1 in all possible slots."
  (let ((my-zeroes (make-zeros-list n))
	(zeros-with-ones nil))
    (dotimes (i n zeros-with-ones)
      ;;; instead of the following, I should check if the current element is 0 and on ly then subst and push. At least for the next step.
      (push (substitute 1 0 my-zeroes :start i :count 1) zeros-with-ones))))
|#    

(defun changes-tower-prob (n)
  "create all possibilities with one +1, one -1 and rest 0s. Turn it into a list with probabilities and return.
   this is essentially all permutations of (1 -1 0 ... 0).
   the probabilities are all equal, this assumes a symmetry. if the symmetry does not exist, obviously a more complicated routine is required."
  (let* ((change-list (unique-permutations
		       (list* 1 -1 (make-list (- n 2) :initial-element 0))))
	 (ll (length change-list)))
    (mapcar #'(lambda (x) (list (/ 1 ll) x)) change-list)))

(defun results-from-change (w change-f)
  "given a state w, calculate all possible results from changes, with probabilities. identify duplicates and combine them, while adding their probabilities.
  this also requires/accepts a function to calculate the list of changes"
  (let* ((n (length w))
	 (change-list (funcall change-f n))
	 (raw-results (loop for (a b) in change-list
			    collect (list a (mapcar #'+ w b)))))
    (format t "~a" raw-results)))
#|
(defun deneme (w change-f)
  (progn
    (format t "~a" w) 
    (funcall change-f 3)))

(defun deneme2 (w change-f)
  (let ((n (length w)))
    (let ((change-list (funcall change-f n)))
      (loop for (a b) in change-list
	    collect (list a (mapcar #'+ w b))))))
|#

(defun permutations (list)
  "Return a list of all permutations of the input list.
   Written by ChatGPT."
  (labels ((remove-nth (n list)
             (append (subseq list 0 n) (subseq list (1+ n))))
           (permute (list)
             (if (null list)
                 '(())
                 (mapcan (lambda (i)
                           (mapcar (lambda (p)
                                     (cons (nth i list) p))
                                   (permute (remove-nth i list))))
                         (loop for i from 0 below (length list) collect i)))))
    (permute list)))

(defun unique-permutations (list)
  "Return a list of all unique permutations of the input list.
   Written by ChatGPT."
  (labels ((remove-nth (n list)
             (append (subseq list 0 n) (subseq list (1+ n))))
           (permute (list)
             (if (null list)
                 '(())
                 (mapcan (lambda (i)
                           (mapcar (lambda (p)
                                     (cons (nth i list) p))
                                   (permute (remove-nth i list))))
                         (loop for i from 0 below (length list) collect i)))))
    (let ((seen (make-hash-table :test 'equal))
          (result (permute list)))
      (remove-if-not (lambda (perm)
                       (unless (gethash perm seen)
                         (setf (gethash perm seen) t)
                         t))
                     result))))
