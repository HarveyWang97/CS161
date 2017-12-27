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


;helper function for goal-test
; check if the row contains a box that is not in the goal state
(defun is-safe-row(r)
  (cond ((NULL r) t)
	(t
	 (cond ((= (car r) box) NIL)
	       ( t (is-safe-row (cdr r)))
	       )
	 )
	)
  )

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; call is-safe-row on each row to check whether there is box that
; is not in the goal state
(defun goal-test (s)
  (cond ((NULL s) t)
	(t
	 (cond ((is-safe-row (car s)) (goal-test (cdr s)))
	       (t NIL)
	       )
	 )
	)
	)		     ;end defun


;helper function for get-square
; get the rth row
(defun get-rth-row (S r)
  (cond

   ; the row is out of range
   ( (< r 0) NIL)

   ; happens if r is large to exceed the maximum
   ( (NULL S) NIL)

   ( (= 0 r) (car S))

   ( t (get-rth-row (cdr S) (- r 1)))
   )
  )


;helper function for get-square
; get the cth column
(defun get-cth-col (x c)
  (cond ( ( NULL x) wall)
	( ( < c 0) wall)
	( ( = c 0) (car x))
	( t (get-cth-col (cdr x) (- c 1)))
	)
  )


; first locate the row, then locate the specific value in the col
(defun get-square (S r c)
 (let ( (x (get-rth-row S r)) )
	 
   ( get-cth-col x c)
   )
  )

; set a square in a specific row
(defun set-row (S c v)
  (cond ( (NULL S) NIL)
	( (< c 0) S)
	( ( = c 0) (cons v (cdr S)))
	( t (cons (car S) (set-row (cdr S) (- c 1) v)))
	)
  )

; locate the row, and use set-row to set the specific value
(defun set-square ( S r c v)
  (cond ( (NULL S) NIL)
	( (or (< r 0) (< c 0)) S)
	( ( > r 0) ( cons (car S) (set-square (cdr S) (- r 1) c v)))
	( t
	  (cons (set-row (car S) c v) (cdr S))
	  )
	)
  )

; change the square into keeper
; if it's originally blank  -> keeper; originally star ->keeper star
(defun change-keeper(S x y)
  (cond 
  	(( = (get-square S x y) boxstar) (set-square S x y keeperstar))
  	(( = (get-square S x y) star)  (set-square S x y keeperstar))
	( t (set-square S x y keeper))
	)
  )

;change the square into box
; if it's originally bkank-> box ; originally star -> box star
(defun change-box(S x y)
  (cond ( (= (get-square S x y) star) (set-square S x y boxstar))
	( t (set-square S x y box))
	)
  )


; x,y -> the position of the keeper
;c1,c2-> the next and the next next boxes' content in the keeper's direction
; first step: check whether the keeper can move
; second step: if the keeper can move, change the position he was at  into blank/star
(defun update-ori-keeper-state( S x y c1 c2)
  (cond ( (= c1 wall) NIL)
	( (and (or (= c2 wall) (= c2 boxstar) (= c2 box))  ( or (= c1 boxstar) (= c1 box))) NIL)
	( t (cond ( (= keeper (get-square S x y)) (set-square S x y blank))
		  ( (= keeperstar (get-square S x y)) (set-square S x y star))
		  )
	    )
	)
  )

;x,y-> the keeper's old position
; after the keeper mvoes, update the content of square the keeper current is(new position)
; for d: 1->up; 2->down; 3->left; 4->right
(defun update-new-keeper-state(S x y d)
  (cond ( (NULL S) NIL)
	( ( = d 1) (change-keeper S (- x 1) y))
	( ( = d 2) (change-keeper S (+ x 1) y))
	( ( = d 3) (change-keeper S  x (- y 1)))
	( (= d 4) (change-keeper S x (+ y 1)))
	)
  )


;x,y-> the keeper's  position
;d : 1->up; 2->down; 3->left; 4->right
; get the content of next squares on the keepers' direction;
; return a list of these two contents
(defun get-content-on-direction (S x y d)
  ( cond ( (= d 1) (list (get-square S (- x 1) y) (get-square S (- x 2) y)))
	 ( ( = d 2) (list (get-square S (+ x 1) y) (get-square S (+ x 2) y)))
	 ( ( = d 3) (list (get-square S x (- y 1)) (get-square S x (- y 2))))
	 ( (= d 4) ( list (get-square S x (+ y 1)) (get-square S x (+ y 2))))
	 )
  )

; x,y -> the keeper's position
; c1,c2-> the next and the next next boxes' content in the keeper's direction
; check whether a box is moved 
(defun need-update-box(S x y c1 c2)
  (cond ( ( and (or (= c1 box) (= c1 boxstar)) (or (= c2 blank) (= c2 star))) t)
	( t NIL)
	)
  )

;x,y -> keeper's position
;change the box's position and update the content of the square
(defun update-box(S x y d)
  ( cond ( (NULL S) NIL)
	 ( t (cond ( (= d 1) (change-box s (- x 2) y))
		   ( (= d 2) (change-box s (+ x 2) y))
		   ( (= d 3) (change-box s x (- y 2)))
		   ( (= d 4) (change-box s x (+ y 2)))
		   )
	     )
	 )
  )




;For d: 1:up 2:down 3:left 4:right
; first: get the keeper's position;
;second : get the content of next two squares on the keeper's direction
;third: change the state of the keeper's old position, then change the state of keeper's new pos
; fourth: check whether a box is moved, if it is, change the state of box's new position
(defun try-move (S d)
( let* ( (x (cadr (getkeeperposition S 0))) (y (car (getkeeperposition S 0))))
    (let* ( (c1 (car (get-content-on-direction S x y d))) ( c2 (cadr (get-content-on-direction S x y d))))
      (let* ((tmp (update-new-keeper-state  (update-ori-keeper-state S x y c1 c2) x y d)))
	(cond ( (or (NULL (need-update-box S x y c1 c2)) (NULL tmp)) tmp)
	      ( t (update-box tmp x y d))
	      )
	)
      )
    )
)
 


;EXERCISE: Modify this function to return the list of 
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
; simply list up the states from moving to 4 directions
(defun next-states (s)
    (cleanuplist (list (try-move s 1) (try-move s 2) (try-move s 3) (try-move s 4)))
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; helper function for h1
; count misplaced boxes in each row
(defun count-misplaced-in-row (r)
  (cond
        ( (NULL r) 0)
        ( (= (car r) box) (+ 1 (count-misplaced-in-row (cdr r))))
	( t (count-misplaced-in-row (cdr r)))
	)
  )


; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; call count-misplaced-in-row on each row
; sum up all the results to get the total num
; this is admissible; suppose there are x mispalced boxes
; we will need at least x moves to move all of them into goal
; thus, h1(s) <= the cost of moving all boxes into the goal
; never overestimate the cost
(defun h1 (s)
  (cond ( (NULL s) 0)
	( t (+ (count-misplaced-in-row (car s)) (h1 (cdr s))))
	)
  )


; return a list of boxes' positions in a row that are not in goal
(defun box-in-row(S r c)
  (cond ( (NULL S) NIL)
	( ( = (car S) box)  (cons (list r c) (box-in-row (cdr S) r (+ 1 c))))
	( t (box-in-row (cdr S) r (+ 1 c)))
	)
  )

; return a list of boxes' positions that are not in goal
(defun list-of-box(S r)
  (cond ( (NULL S) NIL)
	( t  (append (box-in-row (car S) r 0) (list-of-box (cdr S) (+ 1 r))))
	)
  )

(defun star-in-row(S r c)
 (cond ( (NULL S) NIL)
	( (or (= (car S) keeperstar)( = (car S) star))  (cons (list r c) (star-in-row (cdr S) r (+ 1 c))))
	( t (star-in-row (cdr S) r (+ 1 c)))
	)
 )

; return a list of stars' position that are not occupied by boxes
(defun list-of-star (S r)
 (cond ( (NULL S) NIL)
	( t  (append (star-in-row (car S) r 0) (list-of-star (cdr S) (+ 1 r))))
	)
 )

;return the absolute value
(defun absolute(x)
  ( cond ( ( = x 0) x)
	 ( ( < x 0) (- 0 x))
	 ( (> x 0) x)
	 )
  )

; calculate the manhattance distance
(defun manhattan-distance(x y)
  (+ (absolute (- (car x) (car y))) (absolute (- (cadr x) (cadr y))))
  )


; given a single box and a list of goal positions
; find the min distance; we need to pass in x as a very large number for comparision, I will choose 999 in this case
(defun find-min-distance(pos-box list-star x)
  (cond ( (NULL list-star) x)
	( (< (manhattan-distance pos-box (car list-star)) x) (find-min-distance pos-box (cdr list-star) (manhattan-distance pos-box (car list-star))))
	( t (find-min-distance pos-box (cdr list-star) x))
	)
  )

; the sum of min distance given a list of boxes and a list of goals
(defun sumofdistance (box-list star-list)
  (cond ( (NULL box-list) 0)
	( t (+ (find-min-distance (car box-list) star-list 999) (sumofdistance (cdr box-list) star-list)))
	)
  )


; judge whether this box is in the corner
; which means that it can not be moved any more
(defun in-corner(S box)
  (let* ( (x (car box)) (y (cadr box)))
   (cond  ( (and (iswall (get-square S (- x 1) y)) (iswall (get-square S x (- y 1))))  t)
	  ( (and (iswall (get-square S (+ x 1) y)) (iswall (get-square S x (+ y 1))))  t)
	  ( (and (iswall (get-square S (+ x 1) y)) (iswall (get-square S x (- y 1))))  t)
	  ( (and (iswall (get-square S (- x 1) y)) (iswall (get-square S x (+ y 1))))  t)
	  ( t NIL)
	  )
    )
   )
  
; scan the list of boxes to find whether there are boxes in the corner
; if there are, then this situation cannot be solved;
; if cannot be solved, give it a large heuristic value
; I choose 4000 in this case
(defun stuck(S box-list)
  ( cond ( (NULL box-list) 0)
	 ( (equal t (in-corner S (car box-list))) 4000)
	 ( t (stuck S (cdr box-list)))
	 )
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

;my heuristic function is a combination of manhattan distance and stuck situation
; if the situation is stuck, then the number will be very large that it will not be explored anymore
; also, we grade on how close the boxes are to their nearest goals
; this function is admissible because to move a box to the goal, the manhattance distance
; is the least cost, so it never overestimates
(defun h204613387 (s)
  (let* ( (box-list (list-of-box s 0)) (star-list (list-of-star s 0)))
    (+ (sumofdistance box-list star-list) (stuck S box-list))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
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
