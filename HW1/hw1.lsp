;;; Takes an integer as an input and output its padovan number.
(defun SEQ (n)
    (cond ((= n 0) 1) ;;;; padovan number for 0 is 1
          ((= n 1) 1) ;;;; padovan number for 1 is 1
          ((= n 2) 1) ;;;; padovan number of 2 is 1
          (t (+ (SEQ (- n 1)) ;;;; while true, we add (n-1)'s padovan number, (n-2)'s padovan number, and (n-3)'s padovan number
                (+ (SEQ (- n 2))
                   (SEQ (- n 3)))))))

;;; Takes an integer an as input and output the number of additions we need to get its padovan number.
(defun SUMS (n)
    (cond ((= n 0) 0) ;;;; number of additions we need to get 0's padovan number is 0
          ((= n 1) 0) ;;;; number of additions we need to get 1's padovan number is 0
          ((= n 2) 0) ;;;; number of additions we need to get 2's padovan number is 0
          (t (+ 2 (+ (SUMS (- n 1)) ;;;; while true, the number of additions we need to get n's padovan number is two additions plus the numbers of additions need for calculating (n-1)'s, (n-2)'s, and (n-3)'s padovan numbers.
                     (+ (SUMS (- n 2))
                        (SUMS (- n 3))))))))

;;; Takes a TREE list and return a TREE with the same structure but replace all elements with 0s.
(defun ANON (x)
    (cond ((not x) nil) ;;;; if the TREE is empty, we return false
          ((atom x) 0) ;;;; if x is an atom, replace that with 0
          (t (cons (ANON (car x)) (ANON (cdr x)))))) ;;;; run ANON on the first item and the remaining part of the list, then cons them together to make a new list with 0s
