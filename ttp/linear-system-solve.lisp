;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
;;;; Copyright 1992 Patrick H. Winston.  All rights reserved.
;;;; Version 1.1, copied from master file on 21 Jan 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston (licensor) for
;;;; instructional use with the textbooks ``Artificial Intelligence,'' by
;;;; Patrick H. Winston, and ``Lisp,'' by Patrick H. Winston and Berthold
;;;; K. P. Horn.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

;;;; REMARKS

#|

Solves linear equations by row reduction.  
Doubtlessly has terrible numeric properties.

|#

;;;; USER-LEVEL PROCEDURE

(defun solve-equations (rows &aux solution)
  "
  Purpose:	Solve linear equations.
  Arguments:	Rows, in the following form:
		((c11 c12 c13 c1)
		 (c21 c22 c23 c2)
		 (c31 c32 c33 c3))
		where
		  c11 x + c12 y + c13 z = c1
		  c21 x + c22 y + c23 z = c2
		  c31 x + c32 y + c33 z = c3
  Returns:	(x y z)
  "
  (setf rows (mapcar #'(lambda (e) (append e nil)) rows))
  (dotimes (n (length rows))
    (setf (nth n rows)
	  (scale-row (/ (float (nth n (nth n rows)))) (nth n rows)))
    (dotimes (m (length rows))
      (unless (= m n)
	(setf (nth m rows)
	      (add-two-rows (nth m rows)
			    (scale-row (- (nth n (nth m rows)))
				       (nth n rows)))))))
  (setf solution (apply #'append (mapcar #'last rows)))
  solution)

;;;; AUXILIARIES

(defun add-two-rows (r1 r2) (mapcar #'+ r1 r2))

(defun scale-row (c r) (mapcar #'(lambda (e) (* c e)) r))

