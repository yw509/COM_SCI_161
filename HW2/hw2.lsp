;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code

(defun DFSRL (FRINGE)
    (if (null FRINGE) ;;; if FRINGE is null, return the empty list
        '()
        (if (listp (car FRINGE)) ;;; check whether the first item in FRINGE is a list
            ;;; if yes, we run the DFSRL on both the first and the following items, with the first part at the end since we want the reverse order
            (append (DFSRL (cdr FRINGE)) (DFSRL (car FRINGE)))
            ;;; if the first item is an atom, we create a list for that and append with the DFSRL results on the following items
            (append (DFSRL (cdr FRINGE)) (list (car FRINGE))))))


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;

; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call
; (DFS '(NIL NIL NIL NIL) NIL)
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.

(defun FINAL-STATE (S)
    (equal '(T T T T) s))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer
; with dog, and p for homer with poison).
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

(defun NEXT-STATE (S A)
    (cond ((equal A 'h) ;;; if homer is moving
			     (cond
               ;;; we need to check that homer and baby are on the same side, while dog and poison are on the other side.
               ;;; in this way, when homer moves to another side, dog and poison will not threaten the baby.
               ;;; only change the value of homer.
				       ((and (not (equal (second S) (third S))) (not (equal (second S) (fourth S))) (equal (first S) (second S))) (list (list (not (first S)) (second S) (third S) (fourth S))))
               ;;; if homer and baby are on the different sides, since this is a valid state, dog and poison must be on the same side with homer.
				       ((not (equal (first S) (second S))) (list (list (not (first S)) (second S) (third S) (fourth S))))
				       (t NIL)))

           ((equal A 'b) ;;; if baby is moving
            ;;; we need to check that homer an baby are on the same side, so that they can move together.
            ;;; since the baby will not be alone with dog and poison after moving, we do not have to check the position of dog and poison
            (if (equal (first S) (second S))
                (list (list (not (first S)) (not (second S)) (third S) (fourth S)))
                NIL))

           ((equal A 'd) ;;; if dog is moving
            ;;; we need to check that homer and dog are on the same side, so that they can move together.
            ;;; since the baby cannot be alone with poison, we have to make sure that they are not on the same side
            (if (and (equal (first S) (third S)) (not (equal (second S) (fourth S))))
                (list (list (not (first S)) (second S) (not (third S)) (fourth S)))
                NIL))

           ((equal A 'p) ;;; if poison is moving
            ;;; we need to check that homer and poison are on the same side, so that they can move together.
            ;;; since the baby cannot be alone with dog, we have to make sure that they are not on the same side
            (if (and (equal (first S) (fourth S)) (not (equal (second S) (third S))))
                (list (list (not (first S)) (second S) (third S) (not (fourth S))))
                NIL)) ))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

(defun SUCC-FN (S)
    ;;; get all the possible successors and append them together
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.

(defun ON-PATH (S STATES)
    (cond ((null STATES) NIL) ;;; if the STATES is empty, return NIL
          ((equal S (car STATES)) t) ;;; check whether the current state is equal to the visited states one by one
          (t (ON-PATH S (cdr STATES)))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.

(defun MULT-DFS (STATES PATH)
    (cond ((null STATES) NIL)
          ;;; do the depth-first search on each element, if that finds the final state, we run the DFS to return the complete path.
          ((DFS (car STATES) PATH) (DFS (car STATES) PATH))
          ;;; if DFS does not find the final state, we will try running the MULT-DFS on the next legal state.
          (t (MULT-DFS (cdr STATES) PATH))) )

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

(defun DFS (S PATH)
          ;;; first we need to check that whether S is already the goal state, if yes, we apppend the current state to the path, which is the complete path.
    (cond ((FINAL-STATE S) (append PATH (list S)))
          ;;; if we have already have the current state in path, we are searching in a loop, this will set the PATH to NIL
          ((ON-PATH S PATH) NIL)
          ;;; if the current state is a new state, we will add that to the current path. Then, we will generate its possible successors and rerun the DFS on them.
          (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))))


; I discussed the idea of this homework assignment with Chang Xie. But we write up the codes independently.
