(defun fl-interp (E P)
    (cond 
        ((atom E) E)   ;this includes the case where expr is nil
        (t
            (let ( (f (car E))  (arg (cdr E)) )
                (cond 
                    ; handle built-in functions
                    ((eq f 'if)  (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
                    ((eq f 'null) (null (fl-interp (car arg) P)))
                    ((eq f 'first)  (car (fl-interp (car arg) P)))
	       
        	        ; if f is a user-defined function,
                        ;    then evaluate the arguments 
                        ;         and apply f to the evaluated arguments 
                        ;             (applicative order reduction) 

                    ; otherwise f is undefined; in this case,
                    ; E is returned as if it is quoted in lisp
                    (t E)
                )
            )
        )
    )            
)