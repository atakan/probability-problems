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

