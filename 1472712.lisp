#|
CMPUT 325 Assignment 2
Author: Blaz Pocrnja
Student ID: 1472712

This program is an interpreter for a simple functional program that has the following syntax:

(fl-interp '(f X1...Xn) '((f Y1...Yn = (...) )) )

|#

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
                        (let ( (definition (is_defined f arg P)) ) 
                            (fl-interp (reduce_body (cdr (function_header definition)) (evaluate_args arg P) (function_body definition)) P)
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

#| 

is_defined is a function that takes an atom f, a list arg, and a list P. f represents a function name, arg represents a list of parameters for f,
and P is the program that contains function definitions. is_defined returns the function definition from P if the name and arguments length match, otherwise
it returns NIL.

|#
(defun is_defined (f arg P)
    (cond 
        ((null P) nil)
        ((and (eq f (caar P)) (= (list_length arg) (list_length (cdr (function_header (car P)))))) (car P))
        (t (is_defined f arg (cdr P)))
    )
)

#| 

evaluate_args takes an argument list arg, and a program list P, and returns a list of arguments in its normal form.

|#
(defun evaluate_args (arg P)
    (cond 
        ((null arg) nil)
        (t (cons (fl-interp (car arg) P) (evaluate_args (cdr arg) P)))
    )
)

#| 

reduce_body takes a context, represented by lists n and v, and a function body, and returns the normal form of the body.

If a variable in n is in the body, then the corresponding value for n is found and used to replace that variable.
Ex.
body -> (+ x y)
n-> (x y)
v-> (1 2)
Then body becomes -> (+ 1 2)

|#
(defun reduce_body (n v body)
    (cond
        ((null body) nil)
        ((or (null n) (null v)) body)
        ((not (atom (car body))) (cons (reduce_body n v (car body)) (reduce_body n v (cdr body))))
        ((xmember n (car body)) (cons (find_value (car body) n v) (reduce_body n v (cdr body))))
        (t (cons (car body) (reduce_body n v (cdr body))))
    )
)

#| 

The function xmember returns T if argument Y is a member of list X and returns NIL otherwise.

|#
(defun xmember (X Y)
    (cond 
        ((null X) nil)
        ((equal (car X) Y) t)
        (t (xmember (cdr X) Y))
    )
)

#| 

The function find_value returns a value in v that corresponds to the name in n that matches x. 
Returns NIL otherwise.

|#
(defun find_value (x n v)
    (cond
        ((null n) nil)
        ((equal x (car n)) (car v))
        (t (find_value x (cdr n) (cdr v)))
    )
)

#| 

The function list_length returns the length of a given list L.

|#
(defun list_length (L)
    (cond 
        ((null L) 0)
        (t (+ 1 (list_length (cdr L))))
    )
)

#| 

The function function_header returns the header portion of a function definition.

Ex. (f X1 ... Xn = (...)) is a function definition.
	(f x1 ... xn) is the function header.

|#
(defun function_header (DEF)
    (cond 
        ((eq (car DEF) '=) nil)
        (t (cons (car DEF) (function_header (cdr DEF))))
    )
)

#| 

The function function_body returns the body portion of a function definition.

Ex. (f X1 ... Xn = (...)) is a function definition.
	(...) is the function body.
	
|#
(defun function_body (DEF)
    (cond
        ((eq (car DEF) '=) (cadr DEF))
        (t (function_body (cdr DEF)))
    )
)