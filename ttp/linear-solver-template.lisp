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
