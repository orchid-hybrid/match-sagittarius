;; Here are some of the kinds of instruction sequences which are generated for basic patterns

(compile-pattern 'x) ;=> ((bind x))

(compile-pattern ''x) ;=> ((compare-equal? 'x))

(compile-pattern ''(x y)) ;=> ((compare-equal? '(x y)))

(compile-pattern '`(x y))
;; => ((decons) (compare-equal? 'x)
;;     (decons) (compare-equal? 'y)
;;     (compare-equal? '()))

(compile-pattern '`(,x ,y))
;; => ((decons) (bind x)
;;     (decons) (bind y)
;;     (compare-equal? '()))

