;; This is the interpreter for trees of matching instructions
;; matching instructions are as defined in compile-pattern,
;; and the trees terminate with an (execute <code>) command

;; The interpreter handles scope explicitly and manages a
;; fail continuation to do backtracking.

(define-syntax interpret-tree
  (syntax-rules (bind compare-equal? guard decons execute)
    ((interpret-tree scope () stack failure)
     failure)
    ((interpret-tree scope ((branch (execute <body>) ()) <alternatives> ...) stack failure)
     (let* scope <body>))
    ((interpret-tree scope ((branch (bind <var>) <then>) <alternatives> ...) stack failure)
     (let ((top (car stack))
           (new-stack (cdr stack)))
       (interpret-tree ((<var> top) . scope) <then> new-stack
                      (interpret-tree scope (<alternatives> ...) stack failure))))
    ((interpret-tree scope ((branch (compare-equal? <s-expr>) <then>) <alternatives> ...)
                     stack failure)
     (let ((top (car stack))
           (new-stack (cdr stack)))
       (if (equal? top <s-expr>)
           (interpret-tree scope <then> new-stack failure)
           (interpret-tree scope (<alternatives> ...) stack failure))))
    ((interpret-tree scope ((branch (guard <predicate>) <then>) <alternatives> ...)
                     stack failure)
     (if (let* scope <predicate>)
         (interpret-tree scope <then> stack failure)
         (interpret-tree scope (<alternatives> ...) stack failure)))
    ((interpret-tree scope ((branch (decons) <then>) <alternatives> ...) stack failure)
     (let ((top (car stack))
           (new-stack (cdr stack)))
       (if (pair? top)
           (let ((stack (cons (car top) (cons (cdr top) new-stack))))
             (interpret-tree scope <then> stack failure))
           (interpret-tree scope (<alternatives> ...) stack failure))))))
