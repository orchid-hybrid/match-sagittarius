(define-syntax match
  (syntax-rules (else)
    ((match t (<pat> <body> ...) ... (else <else>))
     (let ((stack (list t)))
       (match-expander (<pat> ...) ((begin <body> ...) ...) stack <else>)))
    ((match t (<pat> <body> ...) ...)
     (match t (<pat> <body> ...) ... (else (error "pattern match fell through"))))))

(define-syntax match-expander
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((pats (car (cdr form)))
	   (results (cadr (cdr form)))
	   (stack (caddr (cdr form)))
	   (fail (cadddr (cdr form)))
	   (%interpret-tree (rename 'interpret-tree)))
       ;; change this to `'(,
       ;; if you want to debug
       `(,%interpret-tree ()
                          ,(compile-patterns pats results)
                          ,stack
                          ,fail)))))
