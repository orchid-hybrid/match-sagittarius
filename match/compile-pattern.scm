;; The syntax of patterns is:
;;
;; <pat>  ::= <var>
;;          | `<qpat>            ; (quasiquote <qpat>)
;;          | '<s-exp>           ; (quote <exp>)
;;          | (<prediate?> <var>)
;;
;; <qpat> ::= <sym> | ()
;;          | ,<pat>             ; (unquote <qpat>)
;;          | (<qpat> . <qpat>)

(define (var? s) (symbol? s))
(define (duad? s) (and (list? s) (= 2 (length s))))
(define (quoted? s) (and (duad? s) (eq? 'quote (car s))))
(define (quasiquoted? s) (and (duad? s) (eq? 'quasiquote (car s))))
(define (predicated-var? s) (and (duad? s) (not (quoted? s)) (not (quasiquoted? s)))) ;; so this isn't properly hygienic..
(define (unquoted? s) (and (duad? s) (eq? 'unquote (car s))))
(define (atomic? s) (or (symbol? s) (char? s) (null? s)))


;; compile-pattern turns a <pat> into a list of matching instructions
;; compile-quasipattern turns a <qpat> into a list of matching instructions

;; matching instructions are:
;;
;; <m> ::= (bind <var>)
;;       | (compare-equal? <form>)
;;       | (decons)

(define (compile-pattern pat)
  (cond ((var? pat) (list `(bind ,pat)))
	((quasiquoted? pat) (compile-quasipattern (cadr pat)))
	((quoted? pat) (list `(compare-equal? ',(cadr pat))))
        ((predicated-var? pat) (list `(bind ,(cadr pat)) `(guard ,pat)))
	(else (error "Invalid pattern"))))

(define (compile-quasipattern qpat)
  (cond ((atomic? qpat) (list `(compare-equal? ',qpat)))
	((unquoted? qpat) (compile-pattern (cadr qpat)))
	((pair? qpat)
	 (cons '(decons) (append (compile-quasipattern (car qpat))
				 (compile-quasipattern (cdr qpat)))))
	(else (error "Invalid quasipattern"))))


;; To compile a complex pattern match we make a trie out of the
;; instruction sequences from each individual pattern. This
;; lets the pattern matcher share work, optimizing away redundant
;; tests.

(define (compile-patterns patterns bodies)
  (merge (map (lambda (pattern body)
		(append (compile-pattern pattern) (list `(execute ,body))))
	      patterns bodies)))
