#|
(let ((matrix (grid:make-foreign array 'double-float :initial-contents
				 '((-34.5   8.24   3.29)
				   ( 34.12 -6.15  49.27)
				   ( 32.5  42.73 -17.24))))
      (vec (grid:make-foreign array 'double-float :initial-contents
			      '(-39.66 -49.46 19.68))))
  (multiple-value-bind (matrix perm)
      (lu-decomposition matrix)
    (let ((x (lu-solve matrix vec perm)))
      (grid:copy-to
       (permute-inverse perm
			(matrix-product-triangular matrix
						   (matrix-product-triangular
						    matrix x 1 :upper :notrans
							       :nonunit)
						   1 :lower :notrans
						   :unit))))))
|#

;;;; Taken from https://rosettacode.org/wiki/Gaussian_elimination#Common_Lisp
;;;; Note: it also has LU decomposition https://rosettacode.org/wiki/LU_decomposition#Common_Lisp

(defmacro mapcar-1 (fn n list)
 "Maps a function of two parameters where the first one is fixed, over a list"
  `(mapcar #'(lambda (l) (funcall ,fn ,n l)) ,list) )

(defun gauss (m)
  (labels 
    ((redc (m) ; Reduce to triangular form
       (if (null (cdr m))
         m
        (cons (car m) (mapcar-1 #'cons 0 (redc (mapcar #'cdr (mapcar #'(lambda (r) (mapcar #'- (mapcar-1 #'* (caar m) r) 
                                                                                            (mapcar-1 #'* (car r) (car m)))) (cdr m)))))) ))
     (rev (m) ; Reverse each row except the last element
       (reverse (mapcar #'(lambda (r) (append (reverse (butlast r)) (last r))) m)) ))
    (catch 'result
      (let ((m1 (redc (rev (redc m)))))
        (reverse (mapcar #'(lambda (r) (let ((pivot (find-if-not #'zerop r))) (if pivot (/ (car (last r)) pivot) (throw 'result 'singular)))) m1)) ))))
