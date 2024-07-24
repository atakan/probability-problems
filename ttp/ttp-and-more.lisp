(defparameter *init-cond* '(3 2 2))

(defvar *changes* '((1/6) (-1 1 0)
		    (1/6) (-1 0 1)))

(defun make-zeros-list (n)
  (loop for i from 1 to n collect 0))

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
   this is essentially all permutations of (1 -1 0 ... 0)."
  (let ((seed-list (cons 1 (cons -1 (make-zeros-list (- n 2))))))
    (unique-permutations seed-list)))
  
(defun deniz-bas (n)
  (format t "~:d:" n)
  (format t "~r~%" n))

(defun deniz-say (n)
  (dotimes (i n "bitti")
    (deniz-bas i)))

(defun deniz-say2 (n m)
  (dotimes (i (- m n) "bitti")
    (deniz-bas (+ i n))))

(defun deniz-carp (m n)
  (dotimes (i n "bitti")
    (deniz-bas (expt m i))))

(defun permutations (list)
  "Return a list of all permutations of the input list."
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
