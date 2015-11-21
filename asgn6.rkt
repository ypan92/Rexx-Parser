#lang plai-typed


(require plai-typed/s-exp-match)
(print-only-errors true)

(require (typed-in racket/base
                   [string->number : (string -> number)]))

(require (typed-in racket/list
                   [fifth : ((listof 'a) -> 'a)]))


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [binopC (l : ExprC) (op : symbol) (r : ExprC)]
  )



(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))



;parses through a listof chars representing a rexx program. Creates a list of strings where
;each string represents an expression/line of code.
;expr is a placeholder for an individual expression being constructed
;master-list is the list of strings that contains the strings of expressions/lines of code
(define (create-expressions [prog : (listof char)] [expr : (listof char)] [master-list : (listof string)]) : (listof string)
  (cond
    [(empty? prog)
     (cond
       [(empty? expr) master-list]
       [else
        (local [(define new-expr (list->string expr))
                (define new-master-list (append master-list (list new-expr)))]
          new-master-list)])]
    [(or (eq? (first prog) #\newline) (eq? (first prog) #\;))
     (local [(define new-expr (list->string expr))
             (define new-master-list (append master-list (list new-expr)))]
       (create-expressions (rest prog) empty new-master-list))]
    [else
     (local [(define partial-expr (append expr (list (first prog))))]
       (create-expressions (rest prog) partial-expr master-list))]))
    

;parses a list of chars representing an expression into an ExprC
(define (parse-expr [l : (listof char)]) : ExprC
  (cond
    [(equal? (list->string (list (third l))) "=")
     (binopC
      (idC (string->symbol (list->string (list (first l)))))
      '=
      (parse-expr (rest (rest (rest (rest l))))))]
    [(equal? (list->string (list (third l))) "+")
     (binopC
      (numC (string->number (list->string (list (first l)))))
      '+
      (numC (string->number (list->string (list (fifth l))))))]
    [(equal? (list->string (list (third l))) "-")
     (binopC
      (numC (string->number (list->string (list (first l)))))
      '-
      (numC (string->number (list->string (list (fifth l))))))]
    [(equal? (list->string (list (third l))) "*")
     (binopC
      (numC (string->number (list->string (list (first l)))))
      '*
      (numC (string->number (list->string (list (fifth l))))))]
    [(equal? (list->string (list (third l))) "/")
     (binopC
      (numC (string->number (list->string (list (first l)))))
      '/
      (numC (string->number (list->string (list (fifth l))))))]
))

;parses a listof strings into an equivalent list of ExprC
(define (parse-expr-list [expr-list : (listof string)] [AST-list : (listof ExprC)]) : (listof ExprC)
  (cond
    [(empty? expr-list) AST-list]
    [else
     (local [(define term (parse-expr (string->list (first expr-list))))]
       (parse-expr-list (rest expr-list) (append AST-list (list term))))]))
     
;parses a string representing a rexx program into a list of ExprC
;where each element in the list is a line of code from the rexx program
(define (parse-prog [str : string]) : (listof ExprC)
  (local [(define prog (string->list str))
          (define expr-list (create-expressions prog empty empty))]
    (parse-expr-list expr-list empty)))
 


;string of a simple rexx program
(define testprog1 "x = 5 * 3\n4 + 2")

(test (parse-prog testprog1)
      (list (binopC (idC 'x) '= (binopC (numC 5) '* (numC 3))) (binopC (numC 4) '+ (numC 2))))
