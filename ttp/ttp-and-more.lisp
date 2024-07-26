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
      ;;; instead of the following, I should check if the current element is 0 and only then subst and push. At least for the next step.
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
  "given a state w, calculate all possible results from changes, with probabilities. identify duplicates and combine them (by sorting), while adding their probabilities.
  this also requires/accepts a function to calculate the list of changes.
  example usage:
  CL-USER> (results-from-change '(4 2 2) #'changes-tower-prob)
"
  (let* ((n (length w))
	 (change-list (funcall change-f n))
	 (raw-results (loop for (a b) in change-list
			    collect (list a (mapcar #'+ w b)))))
    (good-f raw-results)))

(defun good-f (rl)
  "this function takes a list, which consists of lists with first element a fraction (probability) and second element a list (some state). it sorts the second element, then combines terms with identical states into a single element with sum of probabilities and the state."
  (let ((states (make-hash-table :test 'equal))
	(result nil))
    (loop for ele in rl
	  do (setf (second ele) (sort (second ele) #'>))
	     (if (gethash (second ele) states)
		 (incf (gethash (second ele) states) (first ele))
		 (setf (gethash (second ele) states) (first ele))))
    ;(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) states)
    (maphash #'(lambda (k v) (push (list v k) result)) states)
    (return-from good-f result)))
						       
;;;;;;
;;;; utility functions (more general purpose)
;;;;;;

(defun knuth-H (n m)
  "Knuth's algorithm TAOCP v4a, p392, for partitions of n into m parts."
  (if (or (< m 2) (< n m))
      (return-from knuth-H (format t "for partitions, we need n >= m>= 2")))
  (let ((result nil)
	(a (make-list (1+ m) :initial-element 1))
	(j 0) (s 0) (x 0))
    (setf (first a) (1+ (- n m)))
    (setf (car (last a)) -1)
    (tagbody
     H2
       (format t "~a~%" a)
       (push (copy-list a) result)
       (if (>= (second a) (- (first a) 1))
	   (go H4))
     H3
       (decf (first a))
       (incf (second a))
       (go H2)
     H4
       (format t "bb ~%")
       (setf j 3)
       (format t "cc ~%")
       (setf s (+ (first a) (second a) -1))
       (format t "dd ~%")
       (loop
	 while (>= (nth (1+ j) a) (- (first a) 1))
	 do (setf s (+ s (nth (1+ j) a)))
	    (incf j))
     H5
       (format t "aa ~a" j)
       (if (> j m) (go END-H))
       (setf x (1+ (nth (1+ j) a)))
       (setf (nth (1+ j) a) x)
       (decf j)
     H6
       (loop
	 while (> j 1)
	 do (setf (nth (1+ j) a) x s (- s x) j (- j 1)))
       (setf (first a) s)
       (go H2)
     END-H)
    (return-from knuth-H result)))
	       	   
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
