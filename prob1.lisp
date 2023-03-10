(defun sim6-2 (&optional (nN 100))
  "calls the routine below repeatedly and averages the results."
  (/ (loop for i from 0 below nN
	   sum (make-random-sample)) nN)) 

(defun make-random-sample-old ()
  "creates random numbers in range [0, 1) until their sum exceeds 1.0.
   returns the number of numbers created.
   It feels like this should be done with do, rather than loop."
  (let ((n 0) (Nsum 0))
    (loop
      (incf Nsum (random 1.0))
      (incf n)
      (when (> Nsum 1.0) (return n)))))

(defun make-random-sample ()
  "same as above, but this is more compact."
  (do ((n 0 (1+ n))
       (Nsum 0 (+ (random 1.0) Nsum)))
      ((> Nsum 1.0) n)))
    
