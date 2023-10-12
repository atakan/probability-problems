;;; This is a code for emulating a selection from a bag of 6 black and 6 white items. One way is actually doing it, and another is to approximate is with a method due to James Ernest. I want to get distributions for both and see if I can improve while keeping the method simple.

;;; The functions below do not check for inputs. E.g., random-move-one does not check if l1 or l2 is a list.

(defun remove-nth (n l)
  ;; removes nth element of the list and returns remaining elements
  (let ((ll (length l)))
    ;(format t "~{~a~^, ~}" (butlast l (- ll n)))
    ;(format t " 00 ")
    ;(format t "~{~a~^, ~}" (last l (- ll (+ 1 n))))
    (append (butlast l (- ll n)) (last l (- ll (+ 1 n))))))

(defun random-pop (l)
  ;; removes a random element from a list, returns a list of the element
  ;; and the remaining list
  ;; if input list is nil, returns (nil nil)
  (let ((n (random (length l)))) ; this is in [0, length). nth also uses zero indexing
    (list (nth n l) (remove-nth n l))))

(defun random-move-one (l1 l2)
  ;; removes a random element from l2 and conses it to (front of) l1. returns resulting (l1 l2)
  ;; if l2 is nil, just return (l1 l2)
  (let* ((ll2 (length l2))
	 (n (random ll2)))
    (if l2
	(list (cons (nth n l2) l1)
	      (remove-nth n l2))
	(list l1 l2))))

(defun randomize (l)
  ;; randomizes the elements of the list l, and returns the result.
  ;; this is shuffle and probably Knuth shuffle would have worked better :-D
