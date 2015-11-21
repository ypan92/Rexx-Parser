#lang plai-typed


(require plai-typed/s-exp-match)
(print-only-errors true)



(define-type ExprC
  [numC (n : number)]
  [binopC (l : ExprC) (op : symbol) (r : ExprC)]
  )

;(define-type Value
 ; [numV 

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))



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
    

;(define (parse [str : string]) : ExprC
  