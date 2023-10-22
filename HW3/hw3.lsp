;
; CS161 Hw3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy
; Allegro).
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
;
; In either case, this limitation should not significantly affect your grade.
;
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
;
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; this helper function helps to find that whether there is a box that is not on Goal
(defun find-box (row)
  (cond
    ((null row) nil) ;;;; if we do not find such a box, return false
    ((isBox (car row)) t) ;;; if we find such a box, return true
    (t (find-box (cdr row)))
  )
)

(defun goal-test (s)
  (cond
    ((null s) t) ;;; after looping the whole s, if we do not find a box which is not on Goal, return true
    ((find-box (car s)) nil) ;;; if we find a box in the current row that is not on Goal, return false
    (t (goal-test (cdr s)))
  )
);end defun


; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
;
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
;

(defun next-states (s)
    (cleanUpList (list (try-move s 'up) (try-move s 'down) (try-move s 'left) (try-move s 'right)))
    ;;; consider four directions, make them as a list. Then, remove any nils in this list.
)

; This function is used to return the integer content of state S at square (r, c)
(defun get-square (S r c)
	(cond ((or (null S) (null (car S))) 1) ;;; if we have looped through the whole state but did not find (r, c), (r, c) must be outside the scope. Therefore, we return 1
		    ((or (< r 0) (< c 0)) 1) ;;; when we go out of the bound, return 1
		    ((and (= r 0) (= c 0)) (car (car S))) ;;; when we arrive (r, c), return the integer content at this point
        ((> r 0) (get-square (cdr S) (- r 1) c)) ;;; if we have not reached the rth row, eliminate the first row of S while subtracting one to r
        ((> c 0) (get-square (cons (cdr (car S)) (cdr S)) r (- c 1))) ;;; if we have not reached the cth column, eliminate the first column of S while subtracting one to c
	)
)

; This function is used to find the correct position in rth row, and change that value to v
(defun set-column (r c v)
  (if (= c 0) ;;; if we are at the correct column,
      (cons v (cdr r)) ;;; change that value to v, cons with the following columns
      (cons (car r) (set-column (cdr r) (- c 1) v)) ;;; if we have not reached the cth column, we will contain the current column. And then cons with the following columns
  )
)

; This function is used to set the (r, c) in state S to value v.
(defun set-square (S r c v)
  (cond ((or (null S) (null (car S))) nil) ;;; if we have looped through the whole state but did not find (r, c), return null
        ((= r 0) (cons (set-column (car S) c v) (cdr S))) ;;; if we are in the correct row, the helper function will help us to find the correct column and change that value to v
        (t (cons (car S) (set-square (cdr S) (- r 1) c v))) ;;; if we have not reached the rth row, we will contain the current row. And then cons with the following lines
  )
)

; This function is used to move the box to the new position. If the movement is invalid, return nil
(defun box-move (S p r c)
  (cond ((or (isBlank (get-square S r c)) (isStar (get-square S r c))) ;;; we can only move the box if the moving direction is star or blank
         (let* ((x (car p))
                (y (car (cdr p)))
                (current (if (isBox (get-square S y x)) ;;; for the box, there are only two possibilities: box itself, or box + goal
                             blank ;;; if box is currently by it, after moving the box, the current position will be blank
                             star)) ;;; if box + goal, after moving the box, the current position will be goal
                (update (if (isStar (get-square S r c)) ;;; if the moving direction is a star, after moving the box, it will be box + star
                            boxstar
                            box))) ;;; otherwise, after moving the box, it will be box itself

         (set-square (set-square S r c update) y x current))) ;;; replace the moving direction with updated box, and replace the current position with the status without box
        (t nil) ;;; if the moving direction is not blank or star, the moving will be invalid
  )
)

; This function is used to move the keeper to the new position.
(defun keeper-move (S x y r c)
  (cond ((null S) nil) ;;; if the state is empty, return nil
        (t (let* ((current (if (isKeeper (get-square S y x)) ;;; for the keeper, there are only two possibilities: keeper himself, or keeper + goal
                                blank ;;; if keeper is currently by himself, after moving the keeper, the current position will be blank
                                star)) ;;; if keeper + goal, after moving the keeper, the current position will be goal
                  (update (if (isStar (get-square S r c))
                               keeperstar ;;; if the moving direction is a star, after moving the keeper, it will be keeper + star
                               keeper))) ;;; otherwise, after moving the keeper, it will be keeper himself
           (set-square (set-square S r c update) y x current))) ;;; replace the moving direction with updated keeper, and replace the current position with the status without keeper
  )
)

; This function is used to generate the State after the movement to the stated direction
(defun try-move (S d)
  (let* ((p (getKeeperPosition S 0)) ;;; we want to find the keeper's current position
         (r (car p)) ;;; the y-coordinate of the keeper's position
         (c (car (cdr p))) ;;; the x-coordinate of the keeper's position
         (upkeeper (get-square S (- c 1) r)) ;;; get the integer content of the upper position
         (downkeeper (get-square S (+ c 1) r)) ;;; get the integer content of the down position
         (leftkeeper (get-square S c (- r 1))) ;;; get the integer content of the left position
         (rightkeeper (get-square S c (+ r 1)))) ;;; get the integer content of the right position

        (cond ((equal d 'up) ;;; if moving up
               (cond ((isBlank upkeeper) (keeper-move S r c (- c 1) r)) ;;; if moving direction is blank, we move keeper to that position
                     ((isStar upkeeper) (keeper-move S r c (- c 1) r)) ;;; if moving direction is star, we move keeper to that position
                     ((isBox upkeeper) (keeper-move (box-move S (list r (- c 1)) (- c 2) r) r c (- c 1) r)) ;;; if moving direction is a box, we have to consider the validity of moving a box
                     ((isBoxStar upkeeper) (keeper-move (box-move S (list r (- c 1)) (- c 2) r) r c (- c 1) r))
                     (t nil))) ;;; other objects will make moving the keeper become invalid

              ((equal d 'down) ;;; if moving down
               (cond ((isBlank downkeeper) (keeper-move S r c (+ c 1) r))
                     ((isStar downkeeper) (keeper-move S r c (+ c 1) r))
                     ((isBox downkeeper) (keeper-move (box-move S (list r (+ c 1)) (+ c 2) r) r c (+ c 1) r))
                     ((isBoxStar downkeeper) (keeper-move (box-move S (list r (+ c 1)) (+ c 2) r) r c (+ c 1) r))
                     (t nil)))

              ((equal d 'left) ;;; if moving left
               (cond ((isBlank leftkeeper) (keeper-move S r c c (- r 1)))
                     ((isStar leftkeeper) (keeper-move S r c c (- r 1)))
                     ((isBox leftkeeper) (keeper-move (box-move S (list (- r 1) c) c (- r 2)) r c c (- r 1)))
                     ((isBoxStar leftkeeper) (keeper-move (box-move S (list (- r 1) c) c (- r 2)) r c c (- r 1)))
                     (t nil)))

              ((equal d 'right) ;;; if moving right
               (cond ((isBlank rightkeeper) (keeper-move S r c c (+ r 1)))
                     ((isStar rightkeeper) (keeper-move S r c c (+ r 1)))
                     ((isBox rightkeeper) (keeper-move (box-move S (list (+ r 1) c) c (+ r 2)) r c c (+ r 1)))
                     ((isBoxStar rightkeeper) (keeper-move (box-move S (list (+ r 1) c) c (+ r 2)) r c c (+ r 1)))
                     (t nil))))
  )
)


; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;

; This h1 heuristic is admissible. In this function, we calculated how many boxes that are not on Goal.
; Suppose we have X such boxes, we need at least X steps to reach the goal state since each boxes needs at least one step.
; Therefore, this heuristic will not overestimate the total cost.
(defun h1 (s)
  (if (null s) ;;; if we have looped the whole list, return 0
      0
      (+ (count 2 (car s)) (h1 (cdr s)))) ;;; we count how many boxes are there in the current line, add with the following lines
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

(defun getBoxColumn (s r c)
  (cond ((null s) nil)
  	    ((isBox (car s)) (cons (list r c) (getBoxColumn (cdr s) r (- c 1))))
  	    (t (getBoxColumn (cdr s) r (- c 1)))
  )
)

; This function is used to return all positions of boxes
(defun getBoxPosition (s r c)
  (cond ((null s) nil)
        (t (append (getBoxColumn (car s) r c) (getBoxPosition (cdr s) (- r 1) c)))
  )
)

(defun getStarColumn (s r c)
  (cond ((null s) nil)
  	    ((isStar (car s)) (cons (list r c) (getStarColumn (cdr s) r (- c 1))))
  	    (t (getStarColumn (cdr s) r (- c 1)))
  )
)

; This function is used to return all positions of stars
(defun getStarPosition (s r c)
  (cond ((null s) nil)
        (t (append (getStarColumn (car s) r c) (getStarPosition (cdr s) (- r 1) c)))
  )
)

; This function is used to calculate the distance between two coordinates.
(defun distance (box star)
  (cond ((or (null box) (null star)) 0) ;;; if either of them is empty, return 0
        ((let* ((x (abs (- (car star) (car box)))) ;;; calculate the x-coordinate difference between two points
	              (y (abs (- (cadr star) (cadr box))))) ;;; calculate the y-coordinate difference between two points
                ;;; since the (cdr box) and (cdr star) are lists, we want to return the element in that list
               (+ x y))) ;;; add them together, which will be the steps we need in order to reach the star from box
  )
)

; This function is used to calculate the minimum distance of a box with all targets
(defun mindistance (currentb Starlist l)
  (cond ((and (null Starlist) (null l)) nil) ;;; if we have looped through the whole starlist but did not find any pairs of boxes and targets, return nil
        ((null Starlist) (min l)) ;;; after looping the whole starlist, return the minimum distance in the list
        (t (append l (list (distance currentb (car Starlist))) (mindistance currentb (cdr Starlist) nil)))
        ;;; we will calculate the distance between the current box and every targets, append them together to make a list
  )
)

; This function is used to add minimum values and return the final result
(defun Manhattandistance (Boxlist Starlist)
  (cond ((or (null Boxlist) (null Starlist)) 0) ;;; if we run out of the boxlist or starlist, return 0
        (t
            (if (null (car (mindistance (car Boxlist) Starlist nil))) ;;; if the minimum value we get is nil, treat it as 0
                0)
                (+ (car (mindistance (car Boxlist) Starlist nil)) (Manhattandistance (cdr Boxlist) Starlist)))
                ;;; add the minimum value for the first box with the following boxes
  )
)

; My heuristic aims to calculate the minimum distances of each boxes with all targets, and add them together since there is no overlap.
; Suppose we have two boxes and two stars,
; I will do Min((b1, s1), (b1, s2)) and Min((b2, s1), (b2, s2)). Then, add them together since they do not have overlap.
(defun h805846047 (s)
  (let* ((Boxlist (getBoxPosition s (length s) (length (car s)))) ;;; we first get the positions of boxes
	       (Starlist (getStarPosition s (length s) (length (car s))))) ;;; and the positions of stars
        (Manhattandistance Boxlist Starlist) ;;; we calculate the Manhattandistance between them
  )
)

;I discussed this assignment with Chang Xie. But we write up the codes independently.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 2 1 4 1)
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1)
	   (1 1 1 0 0 1 1 1 1)
	   (1 0 0 0 0 0 2 0 1)
	   (1 0 1 0 0 1 2 0 1)
	   (1 0 4 0 4 1 3 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
