;;;; Some routines to help with linear algebra
;;;; For now, both matrices and vectors are assumed to be lists.
;;;; e.g. A = ((a11 a12 a13 a14)
;;;;           (a21 a22 a23 a24)
;;;;           (a31 a32 a33 a34)
;;;;           (a41 a42 a43 a44))
;;;; Matrices are typically accessed by (m-el A n m &optional pv).


;;; the following parameters are defined for quicktesting
(defparameter *A* '((1 1/2 1/3 1/4)
		    (2 2/2 2/3 2/4)
		    (3 3/2 3/3 3/4)
		    (4 4/2 4/3 4/4)))

(defparameter *b* '(11 12 13 14))

(defparameter *pv* (range (length *A*)))

(defun range (n)
  (loop for i from 1 to n
	collect i))
#|
(defun nnth (n A)
  (nth (1- n) A))
|#

(defmacro nnth (n A)
  `(nth (1- ,n) ,A))

(defun m-el (A n m &key (pv (range (length A))))
  "return the matrix element at (pv n)th row and mth colum. pv is the pivot matrix and shows the indirect index for rows. Everything is 1-indexed, i.e. A11 is the upper left element."
  (nnth m (nnth (nnth n pv) A)))

;;;; XXX the following is not working as I hoped
(defmacro ttt (A n m &key pv)
  `,(unless pv
    `(setf ,pv (range  (length ,A))))
  `(nnth ,m (nnth (nnth ,n ,pv) ,A)))
		 

(defun augment (A b)
  "Augments a matrix A and (column) vector b"
  (loop for row in A
	for i from 0 to (1- (length A))
	collect (append row (list (nth i b)))))

#|
(defun pivot (A pv n)
  "Given a matrix A, (1) find the largest term in the square sub-matrix A-sub,
   where A-sub is determined by starting from nth row and nth column of A, and getting a square matrix as big as possible. 
   It is assumed that the number of rows of A is <= number of columns of A.
   (2) pivot rows and columns of A (not A-sub) so that this largest element is in the (1,1) point of A-sub.
   Columns are pivoted by swapping elements in each row, rows are pivoted by swapping elements in pv (the pivot vector).
   Both these operations are done in place, i.e., destructively.
   The function returns multiple values of A and pv."
)
|#
  
