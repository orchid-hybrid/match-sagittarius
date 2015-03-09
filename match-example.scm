(map load '("match/trie.sld" "match/compile-pattern.sld" "match/interpret-tree.sld" "match/match.sld"))
(import (match match))

(match 'x ('x 'yes) (else 'no)) ;=> yes

(match 'y ('x 'yes) (else 'no)) ;=> no

(define (my-eval exp env)
  (match exp
    (`(int ,i) i)
    (`(add ,a ,b) (+ (my-eval a env) (my-eval b env)))
    (`(abs ,p ,b) (list 'cls exp env))
    (`(var ,a) (cond ((assoc a env) => cdr)
		   (else (error "unbound variable" a))))
    (`(app ,f ,a)
     (match (my-eval f env)
       (`(cls (abs ,p ,b) ,cenv)
	(my-eval b (cons (cons p (my-eval a env)) cenv)))
       (else (error "not a valid function: " f))))
    (else (error "invalid expression: " exp))))

(my-eval '(app (app (abs a (abs b (add (var a) (var b)))) (int 1)) (int 2)) '()) ;=> 3

(define (foo t)
  ;; #;3> (foo '(if a b))
  ;; (funcall b)
  (match t
    (`(if ,b ,t ,e) (list 'if b t e))
    (`(begin . ,rest) (list 'begin rest))
    (`(evil ,x ,x) (list 'evil x))
    (else (list 'funcall t))))

(foo '(if a b)) ;=> (funcall (if a b))


;; Interesting pattern matching test taken from Oleg's page http://okmij.org/ftp/Scheme/macros.html#match-case-simple
(define (int code env)
  (match code
    (`(quote ,x) x)
    (`(let ((,x ,e)) ,body)
     (let ((xv (int e env)))
       (int body (cons (cons x xv) env))))
    (`(lambda () ,body)                 ; thunk
     (lambda () (int body env)))         ; closed over the env
    (`(lambda (,x) ,body)                ; 1-arg function
     (lambda (xv)
       (int body (cons (cons x xv) env))))
                                        ; the general case of lambda is skipped to keep the example small
    (`(lambda ,argl ,body)               ; arglist
     (lambda arglv
       (int body (cons (cons argl arglv) env))))
    (`(,op . ,args)
     (let* ((opv (int op env))
            (argvs (map (lambda (c) (int c env)) args)))
       (apply opv argvs)))
    (x (if (symbol? x) (lookup x env) x))
    ))

(define (lookup x env)
  (cond
   ((assq x env) => cdr)
   (else (error "Can't find " x))))


(define env0
  (list (cons 'display display)
	(cons '+ +)
	(cons '- -)))

(define (run-tests)
  (int 1 env0)                           ; 1
  (int '1 env0)			        ; '1 is the same as 1

  (int '(quote x) env0)                  ; x

  (int '(display 'x) env0)		; x

					;(int '(display x) env0) 		; error: unbound x

  (int '(let ((x (+ 1 2 3))) (display x)) env0) ; 6
  ((int '(lambda () 1) env0))                   ; 1
  ((int '(lambda (x) x) env0) 1)                ; 1
  (((int '(lambda (x) (lambda (y) (+ x y))) env0) 2) 3) ; 5 (test closure)

  ((int '(lambda l (display l)) env0) 1 2 3) ; (1 2 3)
  )



;;; guard tests

(define (g t)
  (match t
    (`(,(symbol? x) ,(number? y)) (list 'symbol-number x y))
    (`(,(number? x) ,(symbol? y)) (list 'number-symbol x y))
    (else 'neither)))

(g '(3 x)) ;=> (number-symbol 3 x)
(g '(y 5)) ;=> (symbol-number y 5)
(g '(e e)) ;=> neither

