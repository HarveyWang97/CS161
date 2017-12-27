; the argument is a number and a list/number, the return type is a boolean.
; Check whether the tree contains this element
; the overall method is to check the value with the root node. If smaller, check the left subtree; Larger, check the right subtree

(defun TREE-CONTAINS (N TREE)
	(cond   ((NULL TREE) NIL)
	        ((listp TREE)
		 (cond ((= N (second TREE)) t )
		       ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
		       ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
		 )

		 )

		(t (equal N TREE))
		)
)

;the argument is a list/number, the return type is a number
;if the argument is a number, return it directly
; if the tree has a right subtree, call this function on the right subtree
; else, return its node value

(defun TREE-MAX (TREE)
  (cond ((numberp TREE) TREE )
	(t
	 (cond ( (not(NULL (third TREE)))  (TREE-MAX (third TREE)) )
	       ( t (second TREE))
	       )
	 )
	)
  )

;the argument is a list/number, the return type is a list
; if the argument is a number, make it a list and return
; else, call this function on left subtree and right subtree, append them together
(defun TREE-ORDER(TREE)
  (cond  ( (NULL TREE) NIL)
	 ( (numberp TREE) (list TREE))
	 ( t ( append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))
  )
)

;argument: a list and two numbers, the return type is a list
; check whether the length is 0
; check whether start = 0, if it is, then head must be included in the result and append this with 
; the result of (calling this function on the remaining tree)
(defun SUB-LIST(L START LEN)
  (cond  ( (= 0 LEN) NIL)
	 (t
	  (cond  ( (= 0 START) (cons  (car L) (SUB-LIST (cdr L) START (- LEN 1))))
		 ( t (SUB-LIST (cdr L) (- START 1) LEN))
		 )
	  )
  )
  )

;argument: a list; return type: a list of two lists
; if the length is even, just call two times and append;
; if length is odd, calculate the length of first half, then the same step
(defun SPLIT-LIST (L)
  (cond 
	((evenp (length L)) (list (SUB-LIST L  0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
        (t
        	(let* ((x (/ (- (length L) 1) 2)))
        		  (list (SUB-LIST L 0 x) (SUB-LIST L x (- (length L) x)))
        		  )
        )
	)
  )


;argument: a tree in the form a list of lists; return type: int
;if the TREE is a number, then it's a leaf node, return 0;
; calculate the height of left subtree and right subtree, pick the max one;
; the llongest path = max+1

(defun BTREE-HEIGHT(TREE)
  (cond ( (not (listp TREE))  0)
	( t
	  (cond ((> (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE))) (+ 1 (BTREE-HEIGHT (car TREE))))
		(t (+ 1 (BTREE-HEIGHT (cadr TREE))))
		)
	  )
	)
  )


; argument: a list ; return type: a Tree in a form of a list of list
; if the list has length 1, just return the number;
; if the list has length 2, just return the list;
;use split-list on the LEAVES, then recursively call the method on two halves and merge them together
(defun LIST2BTREE(LEAVES)
  (cond ( (= (length LEAVES) 1) (car LEAVES))
	( (= (length LEAVES) 2) LEAVES)
	(t
	   (let* ((x (SPLIT-LIST LEAVES)))
	     (list (LIST2BTREE (car x)) (LIST2BTREE (cadr x)))
	     )
	   )
	)
  )


;argument: a list/atom; return type: a list
; if it's not a list, then make it a list and return
; if it's a list, then call this function recursively on two havles and append them together

(defun BTREE2LIST(TREE)
  (cond ( (not (listp TREE)) (list TREE))
	( t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
	)
  )

; argument: Take in two expressions in the form of lists/number
; return t if (equal E1)
; many subcases, like number vs number. null vs non-null;
;compare the car and cadr recursively
(defun IS-SAME(E1 E2)
  (cond
        ( (and (NULL E1) (NULL E2)) t)
	( (and (not (NULL E1)) (NULL E2)) NIL)
	( (and (NULL E1) (not (NULL E2))) NIL)
        ( (and (numberp E1) (numberp E2)) t)
	( (and (not (numberp E1)) (numberp E2)) NIL)
	( (and (numberp E1) (not (numberp E2))) NIL)
	( t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
	)
  )










     
	   




