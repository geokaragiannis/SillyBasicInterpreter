#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; *label-table* is hash table that will store all of the labels
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key #f))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; *function-table* is hash table that will store all of the functions
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key #f))
(define (function-put! key value)
        (hash-set! *function-table* key value))


;; *variable-table* is hash table that will store all of the variables
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key #f))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(variable-put! 'pi 3.141592653589793238462643383279502884197169399)
(variable-put! 'e 2.718281828459045235360287471352662497757247093)
(variable-put! 'inputcount 0)

;put values in function hash table
(for-each
        (lambda (pair)
                (function-put! (car pair) (cadr pair)))

        `(
                (* ,*)
                (+ ,+)
                (- ,-)
                (/ ,/)
                (% ,(lambda (x y) (- x (* (truncate (/ x y)) y))))
                (^ ,expt)
                (abs ,abs)
                (acos ,acos)
                (asin ,asin)
                (atan ,atan)
                (ceil ,ceiling)
                (cos ,cos)
                (exp ,exp)
                (floor ,floor)
                (log ,log)
                (log10 ,(lambda (x) (/ (log x) (log 10.0))))
                (log2 ,(lambda (x) (/ (log x) (log 2.0))))
                (round ,round)
                (sin ,sin)
                (sqrt ,sqrt)
                (tan ,tan)
                (trunc ,truncate) 
                (= ,=)
                (< ,<)
                (> ,>)
                (<> ,(lambda (x y) (not(= x y))))
                (<= ,<=)
                (>= ,>=)
        )

)


(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program) 
    (printf ")~n"))

(define (add-line-to-label prgram line) 
    ;; first checks to see if the cdr is null, in which case there is no
    ;; label so return 0. otherwise, check to see if the cadr is a symbol,
    ;; if so, the symbol is the label so add it to the hash table. otherwise
    ;; return 0
    (if (null? (cdr line))
        0
        [ if (symbol? (cadr line))
              ; store label, the whole line
              (label-put! (cadr line) (get_sub_program prgram (car line)))
              0]
    ) 
)
      
(define (has_label line) 
   (if (null? (cdr line))
        0
        ; if cadr is a label return true
        [ if (symbol? (cadr line)) #t #f]
    ) 
)

;make sure calling function checks whether cdr of line is not null
;(define (interpret line)
 ;   
  ;  (define stmt (if (has_label line) (caddr line) (cadr line) ))

;)


(define (readnumber)
    (let ((object (read)))
        (cond 
            [(eof-object? object) object]
            [(number? object) (+ object 0.0)]
            [else (begin (printf "invalid number: ~a~n" object)
                         (readnumber))]
        )

    )
)

(define (get-keyboard-input vars input-cnt)
    (cond
        ;; done reading the vars, so just update the input count with the number of variables read
        [(null? vars) (variable-put! 'inputcount input-cnt)]
        [ else
            (let ((number (readnumber)))
                (cond
                    [(eof-object? number)
                        (variable-put! 'inputcount -1)
                    ]
                    [else 
                        (cond
                            ;; if its a symbol, then just put it in the variable table
                            [(symbol? (car vars)) (variable-put! (car vars) number)]
                            ;; otherwise the car is a list which is an (array index)
                            [else
                                (cond
                                    [(or (not(variable-get (caar vars))) (not (vector? (variable-get (caar vars)))))
                                        (die `("Array not initialized"))
                                    ]
                                    ;; array is initilized, check to see if the subscript
                                    ;; is in bounds
                                    [else
                                        (cond
                                            ;; subscriript out of bounds -> dies
                                            [(>= (cadar vars) (vector-length (variable-get (caar vars))))
                                                (die `("Array subscript out of bounds"))
                                            ]
                                            ;; subscrtipt is in bounds -> array[subscript] = (cadar var) 
                                            [else
                                                (vector-set! (variable-get (caar vars)) (cadar vars) number)
                                            ]

                                        )    
                                        
                                    
                                    ]

                                )
                            ]
                        )                 
                        (get-keyboard-input (cdr vars) (+ input-cnt 1))
                    ]
                )
            )
        ]
    )

)

(define (evalexpr expr)
    (cond 
        [ (number? expr) (+ expr 0.0)]
        [ (symbol? expr) (variable-get expr) ]
        [ (pair? expr)
            (cond  
                [ (vector? (variable-get (car expr))) 
                    (vector-ref (variable-get (car expr)) (inexact->exact (evalexpr (cadr expr))))
                ]
                [else (apply (function-get (car expr)) (map evalexpr (cdr expr)))]
            )

        ]
        [ else #f ]
    )

)

(define (print-helper stmt)
    (cond
        [ (null? stmt) (newline)]
        [ (or (symbol? (car stmt)) (number? (car stmt)) (list? (car stmt))) 
            (printf "~s " (evalexpr (car stmt))) 
            (print-helper (cdr stmt))
        ]
        [else
            (printf (car stmt))
            (cond
                [ (null? (cdr stmt)) (print-helper (cdr stmt)) ]
                [ else (printf " ~s" (evalexpr (cadr stmt))) (print-helper (cddr stmt))]                          
            )
        ]
    )

)

(define (interpret prgram)
    ;(printf "program: ~s~n" prgram)
    (define line (car prgram))
    (printf "line: ~s~n" line)       

    (cond 
        ;; if cdr of line is null then there is only a line number, so
        ;; only return 
        [ (null? (cdr line)) 
            (cdr prgram)     
        ]
        ;; cdr of the line is not null, so line may fall into one of the cases:
        ;; 1. it has just a label 2. it has a label and a statement 3. it has just a statement
        [ else (cond
            ;; if (has_label and does not have a statement), nothing to execute so 
            ;; just return the cdr
            [ (and (has_label line) (null? (cddr line)))
                (cdr prgram)
            ]
            ;; case 2 and 3 so there is a statement that must be executed.
            [ else 
                (let ((stmt (if (has_label line) (caddr line) (cadr line) )))
                    ;let body
                    ;(printf "Statement: ~s~n" stmt)
                    ;(cdr prgram)
                    ;; go through the cases of all possible types of statements
                    (cond
                        [ (eqv? (car stmt) 'dim)
                            ;(printf "Stmt is dim~n")                         
                            ;; array can either be initialized with a literal number or with another variable
                            ;; In both cases, array name is specified by (caadr stmt) and the dimensionality
                            ;; is specified by the (cadadr stmt)
                            (variable-put! (caadr stmt) (make-vector (inexact->exact (evalexpr(cadadr stmt)))))
                            (cdr prgram)
                        
                        ]

                        [ (eqv? (car stmt) 'let)
                            ;(printf "Stmt is let~n~n")
                            ;; let can either be setting a normal variable or an index of an array
                            ( cond
                                [ (symbol? (cadr stmt)) 
                                     (variable-put! (cadr stmt) (evalexpr (caddr stmt)))
                                ]
                                [ else
                                    (vector-set! (variable-get (caadr stmt)) (inexact->exact (evalexpr(cadadr stmt))) (evalexpr (caddr stmt)))
                                    
                                ]
                            )
                            (cdr prgram)
                        
                        ]

                        [ (eqv? (car stmt) 'goto)
                            ;; need to return the subprogram corresponding to the label following the goto
                            ;; rather than just returnin the cdr of the program
                            ;(printf "Stmt is goto~n~n")
                            ;; watch out, may cause infinite loops in some programs because let is not implemented
                            (label-get (cadr stmt))
                            ;(cdr prgram)
                        
                        ]

                        [ (eqv? (car stmt) 'if)
                            ;(printf "Stmt is if~n~n")
                            (cond
                                [ (evalexpr (cadr stmt)) (label-get (caddr stmt))]
                                [ else (cdr prgram)]
                            )
                            
                                                    
                        ]

                        [ (eqv? (car stmt) 'print)
                            ;(printf "Is print~n")
                            (print-helper (cdr stmt))                            
                            (cdr prgram)
                        
                        ]

                        [ (eqv? (car stmt) 'input)
                            ;(printf "Is input~n~n")
                            (get-keyboard-input (cdr stmt) 0)
                            (cdr prgram)
                        
                        ]
                        [ else 
                            (printf "Stmt is invalid~n~n")
                            (cdr prgram)
                        
                        ]

                    )           
                )
            ]

        )         
            
        ]
          
    )
 
)

(define (interpreter prgram)
   ; (printf "call to interpret prgram: ~s~n" (interpret prgram))
    (define sub_program (interpret prgram))
    ;(printf "sub_program: ~s~n" sub_program)
    ;; if the sub_program is null, then the program has completed interpreting so exit
    ;; otherwise recursively call interpreter on the sub-program
    (if (null? sub_program)
        0
        (interpreter sub_program))
)

;; Takes the entire program as an argument and a line number
;; If the program has n lines and the line_num is l, then the
;; function returns the sub-program from l to n. 
(define (get_sub_program prgram line_num)

    (if (= line_num 1) prgram (get_sub_program (cdr prgram) (- line_num 1)))
)


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        ;; else statement
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile))) 
               (map (lambda(line) (add-line-to-label program line)) program)           
              
               (interpreter program)

               ;(define expr '(+ (* e 4) (* pi 2)))
               ;(printf "Expr: ~s~n" (evalexpr expr))

               ;printing labels
               #|(hash-for-each *label-table*
                    (lambda (key value)
                        (printf "~s : ~s ~n" key value))
               )|#
               (newline)
               ;printing variables 
               (hash-for-each *variable-table*
                    (lambda (key value)
                        (printf "~s : ~s ~n" key value))
               )

               ;(newline)
               ;printing variables
               #|(hash-for-each *function-table*
                    (lambda (key value)
                        (printf "~s : ~s ~n" key value))
               )|#


             ; (write-program-by-line sbprogfile program)
)))

(main (vector->list (current-command-line-arguments)))


