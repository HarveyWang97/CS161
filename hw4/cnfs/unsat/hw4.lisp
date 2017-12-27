
; top-level function
(defun sat? (n delta)
  (explore n delta NIL)
 )


; backtracking method
; if the guess's length equals to n, check whethere the clauses can be satisfied by this guess
; if there is a clause that is NIL(all literals are assigned to false), return NIL directly
; then try the literal one by one, if this assignment works, return the result;
; else, try another assignment(its opposite form)
(defun explore(n delta guess)
  ( cond (  (= (length guess) n)
	   (cond ( (is_correct_all delta guess) guess)
		 ( t NIL)
		 ))
	 ( (check_empty_clause delta) NIL)
	   ( t (let* ( (x (find_next_var delta guess n)))
		 (let* ( (new_list (update_list x delta))
			 (try (explore n new_list (cons x guess))))
		   (cond ( (NULL try) (explore n (update_list (- 0 x) delta) (cons (- 0 x) guess)))
			 (t try)
			 )
		   )
		 )
	       )
	   )
  )
		   
; remove the opposite form of element from a clause
(defun remove_opposite (element clause)
  ( cond ( (NULL clause) NIL)
         ( ( = (- 0 element) (car clause)) (cdr clause))
	 ( t  (cons (car clause)  (remove_opposite element (cdr clause))))
	 )
  )

; given the element, remove the clause that contains the element(judge to true)
; and modify the clause that contains negative form of element(delete the negative form)
(defun update_list( element list)
  ( cond ( (NULL list) nil)
	 ( (contain element (car list))  (update_list element (cdr list)))
	 ( t  (cons (remove_opposite element (car list)) (update_list element (cdr list))))
	 )
  )



(defun contain(element clause)
  (cond ( (NULL clause) NIL)
	( (equal element (car clause)) t)
	( t (contain element (cdr clause)))
	)
  )

; whethere a clause is true
(defun is_correct_single (clause guess)
  (cond( (NULL clause) NIL)
       (  (contain (car clause) guess)   t)
       ( t (is_correct_single (cdr clause)  guess))
       )
  )

; whether all the clauses are true
(defun is_correct_all (clauses guess)
  (cond ( (NULL clauses) t)
	( (equal t (is_correct_single (car clauses) guess)) (is_correct_all (cdr clauses) guess))
	( t NIL)
	)
  )

; find if there is clause NIL (because all literals assign to false, the clause becomes nil)
(defun check_empty_clause (clauses)
  (cond ( (NULL clauses) NIL)
        ( (NULL (car clauses)) t)
	    ( t (check_empty_clause (cdr clauses)))
	)
  )

; find the clause of only one literal
(defun find_unit_clause(list)
  ( cond ( (NULL list) NIL)
	 ( (= (length (car list)) 1) (car list))
	 ( t (find_unit_clause (cdr list)))
	 )
  )

; find the literal to be assigned next according to the absolute value order
(defun get_var_assigned(n guess tmp)
  (cond ( (> tmp n) nil)
	( (or (contain tmp guess) (contain (- 0 tmp) guess)) (get_var_assigned n guess (+ 1 tmp)))
	( t tmp)
	)
  )


; first find the clause of only one literal; if there does not exist one
; assign the literal according to absolute value order
(defun find_next_var(delta guess n )
  (cond ( (not (NULL (find_unit_clause delta))) (car (find_unit_clause delta)))
	( t (get_var_assigned n guess 1))
	)
  )
