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
        (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; *function-table* is hash table that will store all of the functions
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key))
(define (function-put! key value)
        (hash-set! *function-table* key value))


;; *variable-table* is hash table that will store all of the variables
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(variable-put! 'pi 3.141592653589793238462643383279502884197169399)
(variable-put! 'e 2.718281828459045235360287471352662497757247093)

;put values in function hash table
(for-each
        (lambda (pair)
                (function-put! (car pair) (cadr pair)))

        `(

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
               ;printing labels
               (hash-for-each *label-table*
                    (lambda (key value)
                        (printf "~s : ~s ~n" key value))
               )
               (newline)
               ;printing variables
               (hash-for-each *variable-table*
                    (lambda (key value)
                        (printf "~s : ~s ~n" key value))
               )

               (newline)
               ;printing variables
               (hash-for-each *function-table*
                    (lambda (key value)
                        (printf "~s : ~s ~n" key value))
               )

             ; (write-program-by-line sbprogfile program)
)))

(main (vector->list (current-command-line-arguments)))


