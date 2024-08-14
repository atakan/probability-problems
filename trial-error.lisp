;;; guessing a formula by trial/error.
;;; this is mostly to see what kind of macros would be useful/needed.

(defparameter *target* (/ 1234 59999))

(defun te ()
  (loop for i from -2 to 2
	do (print i)))


(defmacro for (var start stop &body body)
  "this is from Paul Graham's book, I think it's written by RTM"
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop , stop))
      ((> ,var ,gstop))
       ,@body)))

(defmacro multi-for (var-list &body body)
  "a macro that creates nested for loops.
   e.g. (multi-for ((a -2 2) (b 0 12))
                   (print a)
                   (print b))
  "
  (labels ((nested-for (vars)
	     (if (null vars)
		 `(progn ,@body)
		 `(for ,@(car vars)
		      ,(nested-for `,(cdr vars))))))
    (nested-for var-list)))
	      
