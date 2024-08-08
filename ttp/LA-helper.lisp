;;;; Some routines to help with linear algebra
;;;; For now, both matrices and vectors are assumed to be lists.
;;;; e.g. A = ((a11 a12 a13 a14)
;;;;           (a21 a22 a23 a24)
;;;;           (a31 a32 a33 a34)
;;;;           (a41 a42 a43 a44))
;;;; Matrices are typically accessed by (m-el A row col).

;;; let's give this a name
(defpackage :la-helper
  (:use :common-lisp))

;;; the following parameters are defined for quicktesting
(defun range (n)
  (loop for i from 1 to n
	collect i))

(defparameter *A* '((   1  1/2  1/3  1/4)
		    ( 2/5  2/9 2/13 2/17)
		    (3/19 3/23 3/29 3/31)
		    (4/37 4/41 4/47 4/53)))

(defun qrm (&key (n 4) (max 1200))
  "quasi-random-matrix"
  (loop for j from 1 to n
	collect (loop for i from 1 to n
		      collect (random max))))

(defparameter *b* '(11 12 13 14))

#|
(defun nnth (n A)
  (nth (1- n) A))
|#

;; defined as macros to be able to setf
(defmacro nnth (n A)
  "1-indexed nth"
  `(nth (1- ,n) ,A))

(defmacro m-el (A row col)
  "return the matrix element at the given row and column.
   Everything is 1-indexed. 
   The matrix is a list of rows, which themselves are lists."
  `(nnth ,col (nnth ,row ,A)))

(defun augment (A b)
  "Augments a matrix A and (column) vector b"
  (loop for row in A
	for i from 0 to (1- (length A))
	collect (append row (list (nth i b)))))

(defun m-pprint (A)
  (let ((n (length A)))
    (loop for i from 1 to n
	  do (print (nnth i A)))))

(defun max-in-column-below (A i)
  "Given a matrix A, find j>=i where jth row has the maximum in ith column.
   This is what is needed for pivoting and this routine is slightly more efficient than the more general one given elsewhere."
  (let ((n (length A))
	(col-max (m-el A i i))
	(col-max-i i))
    (loop for j from i to n
	  do (when (> (abs (m-el A j i)) (abs col-max))
		   (setf col-max (m-el A j i) col-max-i j)))
    (values col-max-i)))

(defun swap-row (A r1 r2)
  "swaps rows r1 and r2 of matrix A.
   r1 and r2 are 1-indexed."
  (rotatef (nnth r1 A) (nnth r2 A)))

(defun scale-v (v s)
  "divides all elements of v by s, returns the result."
  (loop for x in v collect (/ x s)))

(defun normalize-row (A row)
  "divides out the given row of A, by the (row, row) element."
  (let ((divisor (nnth row (nnth row A))))
    (setf (nnth row A) (scale-v (nnth row A) divisor))))

(defun scale-subt (v1 c v2)
  "calculate and return v1-c*v2, where v1 and v2 are vectors of same length."
  (loop for i in v1
	for j in v2
	collect (- i (* c j))))

(defun kill-below-diag (A row)
  "by making linear combinations, make all elements below (row, row) 0.
   We assume that the element (row, row) is 1."
  (let ((n (length A)))
    (loop for i from (1+ row) to n
	  do (setf (nnth i A)
		   (scale-subt (nnth i A) (nnth row (nnth i A)) (nnth row A))))
  (values A)))

(defun back-subst (Ab)
  "given an augmented triangular matrix, do back-substitution and give result."
  (let* ((n (length Ab))
	 (x (make-list n :initial-element 0)))
    (loop for k from n downto 1 ;; we calculate kth x, starting from bottom to up
	  do (setf (nnth k x)   ;; we are at kth row
		   (/ (- (nnth (1+ n) (nnth k Ab)) ;; this is kth b
			 (loop for i from (1+ k) to n ;; all to the right of diag
			       sum (* (nnth i x) (nnth i (nnth k Ab)))))
		      (nnth k (nnth k Ab)))))
  (values x)))

(defun gauss-elim-pp (A b)
  "returns solution vector x.
  1. create augmented matrix 
  2. For every column, (a) find the largest element, pivot it to diagonal
  2b. Normalize the corresponding row
  2c. Make the elements below diagonal zero by linear combination
  3. Obtain the solution by substitution."
  (let ((n (length A))
	(aug-mat (augment A b)))
    (loop for col from 1 to n
	  do (swap-row aug-mat col (max-in-column-below aug-mat col))
	     (normalize-row aug-mat col)
	     (kill-below-diag aug-mat col))
    (back-subst aug-mat)))

(defun print-wvec (v)
  "print a list in wolfram vector form"
  (format t "{")
  (loop for x in (butlast v)
	do (format t "~a, " x))
  (format t "~a}" (car (last v))))

(defun print-wmat (m)
  "print a list of lists in wolfram matrix form"
  (format t "{")
  (loop for v in (butlast m)
	do (print-wvec v)
	   (format t ", "))
  (print-wvec (car (last m)))
  (format t "}"))

(defun print-for-wolfram (A b)
  "print a command for mathematica/wolfram to solve linear system Ax=b.
   e.g. LinearSolve[{{r, s}, {t, u}}, {y, z}]" 
  (format t "LinearSolve[")
  (print-wmat A)
  (format t ", ")
  (print-wvec b)
  (format t "]~%"))

(defparameter *MA* '((  0    0  1/3 1/3 -2/3)
		     (  0  1/3  1/3  -1  1/3)
		     (  0  1/6 -5/6 1/6  1/6)
		     (1/6 -5/6  1/6 1/6    0)
		     ( -1  1/3    0   0    0)))

(defparameter *Mb* '(-1 -1 -1 -1 -1))
