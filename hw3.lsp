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
	);end defun

;helper function for goal-test
;recursively checks if there are any boxes in a given row, and returns true if a single box is found
(defun row-test (row)
	(cond 
		((null row) nil) ;null case
		(t (or (isBox (car row)) (checkRow (cdr row)))) ;returns true if box is found
		);end cond
	);end defun

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;recursively passes each row of its state to row-test to check for any boxes
(defun goal-test (s)
	(cond
		((null s) t) ;null case
		(t (and (not (row-test (car s))) (goal-test (cdr s)))) ;returns true iff all row-test calls return false
		);end cond
	);end defun

;helper function for get-square
;returns the specified element from the given row
(defun get-element (row c)
	(cond
		((null row) nil) ;null case
		((< c 0) wall) ;out of bounds
		((>= c (length row)) wall) ;out of bounds
		(t (car (nthcdr c row))) ;returns specified element from row
		);end cond
	);end defun

;get-square takes in a State S, a row number r, and a column number c
;it returns the integer content of state S at square (r,c)
;if the square is outside the scope of the problem, returns the value of a wall
(defun get-square (S r c)
	(cond
		((null S) nil) ;null case
		((< r 0) wall) ;out of bounds
		((>= r (length S)) wall) ;out of bounds
		(t (get-element (car (nthcdr r s)) c)) ;passes specified row from state to get-element
		);end cond
	);end defun

;helper function for set-square
;replaces the specified element from the given row
(defun set-element (row c v)
	(cond
		((null row) nil) ;null case
		((< c 0) nil) ;out of bounds
		((>= c (length row)) nil) ;out of bounds
		((= c 0) (cons v (cdr row))) ;replace element
		(t (cons (car row) (set-element (cdr row) (- c 1) v))) ;recursively call set-element
		);end cond
	);end defun

;set-square takes in a state S, a row number r, a column number c, and a square content v (integer)
;this function returns a new state S’ that is obtained by setting the square (r,c) to value v
;this function does not modify the input state
(defun set-square (S r c v)
	(cond 
		((null S) nil) ;null case
		((< r 0) nil) ;out of bounds
		((< c 0) nil) ;out of bounds
		((>= r (length S)) nil) ;out of bounds
		((>= c (length (car S))) nil) ;out of bounds
		((= r 0) (cons (set-element (car S) c v) (cdr S))) ;replace row by calling set-element
		(t (cons (car S) (set-square (cdr S) (- r 1) c v))) ;recursively call set-square
		);end cond
	);end defun

;helper function for try-move
;updates the position of the keeper using the given position and surrounding positions
(defun handle-keeper (S r c one two)
	(cond 
		((isWall one) nil) ;wall is one move away
		((and (isBox one) (not (or (isStar two) (isBlank two)))) nil) ;box one move away and obstacle two moves away
		((isKeeper (get-square S r c)) (set-square S r c blank)) ;replace keeper with blank
        ((isKeeperStar (get-square S r c)) (set-square S r c star)) ;replace keeperstar with star
		);end cond
	);end defun

;try-move takes in a state S and a move direction D
;this function returns the state that is the result of moving the keeper in state S in direction D
;NIL is returned if the move is invalid
(defun try-move (S D)
	(let* ((c (car (getKeeperPosition S 0))) (r (car (cdr (getKeeperPosition S 0))))) ;set r and c to keeper position
		(cond
			((equal D 'u) ;up direction
				(let* ((one (get-square S (- r 1) c)) (two (get-square S (- r 2) c)) (k (handle-keeper S r c one two))) ;set one and two to position one and two moves away
					(cond
						((isBlank one) (set-square a (- r 1) c keeper)) ;blank one move away
						((isStar one) (set-square k (- r 1) c keeperstar)) ;star one move away
						((and (isBox one) (isBlank two)) (set-square (set-square k (- r 1) c keeper) (- r 2) c box)) ;box one move away and blank two moves away
						((and (isBox one) (isStar two)) (set-square (set-square k (- r 1) c keeper) (- r 2) c boxstar)) ;box one move away and star two moves away
						((and (isBoxStar one) (isBlank two)) (set-square (set-square k (- r 1) c keeperstar) (- r 2) c box)) ;boxstar one move away and blank two moves away
						((and (isBoxStar one) (isStar two)) (set-square (set-square k (- r 1) c keeperstar) (- r 2) c boxstar)) ;boxstar one move away and star two moves away
						);end cond
					);end let
				);end equal
			((equal D 'r) ;right direction
				(let* ((one (get-square S r (+ c 1))) (two (get-square S r (+ c 2))) (k (handle-keeper S r c one two))) ;set one and two to position one and two moves away
					(cond
						((isBlank one) (set-square k r (+ c 1) keeper)) ;blank one move away
						((isStar one) (set-square k r (+ c 1) keeperstar)) ;star one move away
						((and (isBox one) (isBlank two)) (set-square (set-square k r (+ c 1) keeper) r (+ c 2) box)) ;box one move away and blank two moves away
						((and (isBox one) (isStar two)) (set-square (set-square k r (+ c 1) keeper) r (+ c 2) boxstar)) ;box one move away and star two moves away
						((and (isBoxStar one) (isBlank two)) (set-square (set-square k r (+ c 1) keeperstar) r (+ c 2) box)) ;boxstar one move away and blank two moves away
						((and (isBoxStar one) (isStar two)) (set-square (set-square k r (+ c 1) keeperstar) r (+ c 2) boxstar)) ;boxstar one move away and star two moves away
						);end cond
					);end let
				);end equal
			((equal D 'd) ;down direction
				(let* ((one (get-square S (+ r 1) c)) (two (get-square S (+ r 2) c)) (k (handle-keeper S r c one two))) ;set one and two to position one and two moves away
					(cond
						((isBlank one) (set-square k (+ r 1) c keeper)) ;blank one move away
						((isStar one) (set-square k (+ r 1) c keeperstar)) ;star one move away
						((and (isBox one) (isBlank two)) (set-square (set-square k (+ r 1) c keeper) (+ r 2) c box)) ;box one move away and blank two moves away
						((and (isBox one) (isStar two)) (set-square (set-square k (+ r 1) c keeper) (+ r 2) c boxstar)) ;box one move away and star two moves away
						((and (isBoxStar one) (isBlank two)) (set-square (set-square k (+ r 1) c keeperstar) (+ r 2) c box)) ;boxstar one move away and blank two moves away
						((and (isBoxStar one) (isStar two)) (set-square (set-square k (+ r 1) c keeperstar) (+ r 2) c boxstar)) ;boxstar one move away and star two moves away
						);end cond
					);end let
				);end equal
			((equal D 'l) ;left direction
				(let* ((one (get-square S r (- c 1))) (two (get-square S r (- c 2))) (k (handle-keeper S r c one two))) ;set one and two to position one and two moves away
					(cond
						((isBlank one) (set-square k r (- c 1) keeper)) ;blank one move away
						((isStar one) (set-square k r (- c 1) keeperstar)) ;star one move away
						((and (isBox one) (isBlank two)) (set-square (set-square k r (- c 1) keeper) r (- c 2) box)) ;box one move away and blank two moves away
						((and (isBox one) (isStar two)) (set-square (set-square k r (- c 1) keeper) r (- c 2) boxstar)) ;box one move away and star two moves away
						((and (isBoxStar one) (isBlank two)) (set-square (set-square k r (- c 1) keeperstar) r (- c 2) box)) ;boxstar one move away and blank two moves away
						((and (isBoxStar one) (isStar two)) (set-square (set-square k r (- c 1) keeperstar) r (- c 2) boxstar)) ;boxstar one move away and star two moves away
						);end cond
					);end let
				);end equal
			);end cond
		);end let
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
	(cleanUpList (list (try-move s 'u) (try-move s 'r) (try-move s 'd) (try-move s 'l))) ;combine all possible moves in list
	)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0 ;return 0
	)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
	(cond 
		((null s) 0) ;null case
        ((atom s) (if (isBox s) 1 0)) ;return 1 if box
        (t (+ (h1 (car s)) (h1 (cdr s)))) ;count all the boxes
    	);end cond
  	);end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h105125631 (s)
	nil
  	)

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
