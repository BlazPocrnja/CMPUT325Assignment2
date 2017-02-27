(defun fl-interp (E P)
    (cond 
        ((atom E) E)   ;this includes the case where expr is nil
        (t
            (let ( (f (car E))  (arg (cdr E)) )
                (cond 
                    ; handle built-in functions
                    ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
                    ((eq f 'null) (null (fl-interp (car arg) P)))
                    ((eq f 'atom) (atom (fl-interp (car arg) P)))
                    ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f 'first) (car (fl-interp (car arg) P)))
                    ((eq f 'rest) (cdr (fl-interp (car arg) P)))
                    ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f 'isnumber) (numberp (fl-interp (car arg) P)))
                    ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                    ((eq f 'not) (not (fl-interp (car arg) P)))
	       
        	        ; if f is a user-defined function,
                        ;    then evaluate the arguments 
                        ;         and apply f to the evaluated arguments 
                        ;             (applicative order reduction) 
                    ((is_defined f arg P) 
                        (

                        )
                    )

                    ; otherwise f is undefined; in this case,
                    ; E is returned as if it is quoted in lisp
                    (t E)
                )
            )
        )
    )            
)

(defun is_defined (f arg P)
    (cond 
        ((null P) nil)
        ((and (eq f (caar P)) (= (list_length arg) (list_length (cdr (function_header (car P)))))) t)
        (t (is_defined f arg (cdr P)))
    )
)

(defun list_length (L)
    (cond 
        ((null L) 0)
        (t (+ 1 (list_length (cdr L))))
    )
)

(defun function_header (DEF)
    (cond 
        ((eq (car DEF) '=) nil)
        (t (cons (car DEF) (function_header (cdr DEF))))
    )
)
(defun function_body (DEF)
    (cond
        ((eq (car DEF) '=) (cadr DEF))
        (t (function_body (cdr DEF)))
    )
)