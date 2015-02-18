(map load '("trie.sld" "compile-pattern.sld" "interpret-tree.sld" "match.sld"))

(import (time) (match) (rename (match pattern) (match m:match))
        ;; (math random)
        (srfi :1))

;; (define rng (pseudo-random RC4 :seed 3652248))

(define (randomly n . l)
  #;
  (let ((i (random rng (length l))))
    (list-ref l i))
  ;; produce the same result
  (let ((i (mod n (length l))))
    (list-ref l i)))

(define (g n)
  (if (<= n 1)
      (randomly n 'true 'false)
      (case (randomly n 'unary 'binary)
        ((unary) `(not ,(g (- n 1))))
        ((binary) (let ((p (randomly n 'and 'or)))
                    `(,p ,(g (- n 2)) ,(g (- n 2))))))))


;; Boolean logic

(define (evb t)
  (match t
    ('true 'true)
    ('false 'false)

    (('not 'true) 'false)
    (('not 'false) 'true)
    (('not y) (evb `(not ,(evb y))))
    
    (('and 'true x) (evb x))
    (('and 'false x) 'false) ;; This must be here.. if you put 
    (('and x 'true) (evb x)) ;;;  <- this one first
    (('and x 'false) 'false) ;; you will get an infinite loop
    (('and x y) (evb `(and ,(evb x) ,y)))

    (('or 'true x) 'true)
    (('or 'false x) (evb x))
    (('or x 'true) 'true)
    (('or x 'false) (evb x))
    (('or x y) (evb `(or ,(evb x) ,y)))
    
    ))

(define (m:evb t)
  (m:match t
    ('true 'true)
    ('false 'false)

    (`(not true) 'false)
    (`(not false) 'true)
    (`(not ,y) (m:evb `(not ,(m:evb y))))
    
    (`(and true ,x) (m:evb x))
    (`(and false ,x) 'false) ;; This must be here.. if you put 
    (`(and ,x true) (m:evb x)) ;;;  <- this one first
    (`(and ,x false) 'false) ;; you will get an infinite loop
    (`(and ,x ,y) (m:evb `(and ,(m:evb x) ,y)))

    (`(or true ,x) 'true)
    (`(or false ,x) (m:evb x))
    (`(or ,x true) 'true)
    (`(or ,x false) (m:evb x))
    (`(or ,x ,y) (m:evb `(or ,(m:evb x) ,y)))
    
    ))

(define (repeat lim thing)
  (do ((i 0 (+ i 1)))
      ((= i lim))
    (thing)))

(define (go-b-sanity) ;; this is a test to makre sure both give same results
  (begin
      (repeat 100 (lambda () (let ((t (g 4)))
                               (unless (equal? (evb t) (m:evb t))
                                       (display `(the sky is falling ,t))))))
      (repeat 200 (lambda () (let ((t (g 8)))
                               (unless (equal? (evb t) (m:evb t))
                                       (display `(the sky is falling ,t))))))
      (repeat 300 (lambda () (let ((t (g 16)))
                               (unless (equal? (evb t) (m:evb t))
                                       (display `(the sky is falling ,t))))))
      ))

(define (go-b evb-version)
  ;; (set! rng (pseudo-random RC4 :seed 3652248))
  (time (repeat 100 (lambda () (evb-version (g 4)))))
  (time (repeat 200 (lambda () (evb-version (g 8)))))
  (time (repeat 400 (lambda () (evb-version (g 16)))))
  (time (repeat 800 (lambda () (evb-version (g 32))))))

(go-b evb)
(go-b m:evb)

;; $ rlwrap sagittarius
;; sash> (load "benchmark.scm")
;; GC Warning: Repeated allocation of very large block (appr. size 2101248):
;; 	May lead to memory leak and poor performance.
;; #t
;; sash> (begin (go-b evb) (go-b m:evb))

;; ;;  (begin (repeat 100 (lambda () (evb-version (g 4)))) (repeat 200 (lambda () (evb-version (g 8)))) (repeat 400 (lambda () (evb-version (g 16)))) (repeat 800 (lambda () (evb-version (g 32)))))
;; ;;  6.170166 real    17.279999 user    0.040000 sys

;; ;;  (begin (repeat 100 (lambda () (evb-version (g 4)))) (repeat 200 (lambda () (evb-version (g 8)))) (repeat 400 (lambda () (evb-version (g 16)))) (repeat 800 (lambda () (evb-version (g 32)))))
;; ;;  5.997716 real    16.789998 user    0.016667 sys
;; #t

;; modified result
;; % /opt/bin/sash benchmark.scm

;;  (repeat 100 (lambda () (evb-version (g 4))))
;;  0.000000 real    0.000000 user    0.000000 sys

;;  (repeat 200 (lambda () (evb-version (g 8))))
;;  0.010000 real    0.015000 user    0.000000 sys

;;  (repeat 400 (lambda () (evb-version (g 16))))
;;  0.280000 real    0.375000 user    0.000000 sys

;;  (repeat 800 (lambda () (evb-version (g 32))))
;;  137.366201 real    165.1740 user    4.804000 sys

;;  (repeat 100 (lambda () (evb-version (g 4))))
;;  0.000000 real    0.000000 user    0.000000 sys

;;  (repeat 200 (lambda () (evb-version (g 8))))
;;  0.000000 real    0.000000 user    0.000000 sys

;;  (repeat 400 (lambda () (evb-version (g 16))))
;;  0.150000 real    0.156000 user    0.000000 sys

;;  (repeat 800 (lambda () (evb-version (g 32))))
;;  73.992105 real    79.74700 user    1.389000 sys


;; this matcher is 0.2s faster in the boolean benchmark

;; Measurements:

;; evb
;;  6.011718 real    16.666665 user    0.050000 sys
;;  6.316228 real    17.746665 user    0.006667 sys
;;  6.124855 real    17.249998 user    0.036667 sys
;;  6.158444 real    17.183332 user    0.063334 sys
;;  5.898864 real    16.336665 user    0.053334 sys
;;  6.077455 real    17.099999 user    0.023333 sys

;; m:evb
;;  6.018126 real    16.599998 user    0.016667 sys
;;  5.827293 real    16.013332 user    0.026666 sys
;;  5.928175 real    16.319998 user    0.016667 sys
;;  5.825319 real    15.783332 user    0.023333 sys
;;  5.976858 real    16.643331 user    0.016666 sys
;;  5.694204 real    15.463332 user    0.020000 sys



;; !!!! oops forgot to reset the seed.. added this now..

;; evb
;;  5.840854 real    16.933332 user    0.033333 sys
;;  5.916212 real    17.149999 user    0.023333 sys
;;  5.864864 real    16.999998 user    0.013333 sys
;;  6.070173 real    17.699999 user    0.026666 sys
;;  5.752438 real    16.183332 user    0.020000 sys
;;  5.739199 real    16.149999 user    0.030000 sys


;; m:evb
;;  5.638897 real    16.153331 user    0.016667 sys
;;  5.614863 real    16.119998 user    0.006667 sys
;;  5.716652 real    16.593332 user    0.036667 sys
;;  5.578677 real    15.749998 user    0.013334 sys
;;  5.544080 real    15.539998 user    0.020000 sys
;;  5.607738 real    16.133331 user    0.020000 sys





;; Original benchmark

(define (count-pair lis)
  (let loop ((i 0) (lis lis))
    (match lis
      (((a . d) rest ...)
       (loop (+ i 1) rest))
      ((x rest ...)
       (loop i rest))
      (() i))))
 
(define (m:count-pair lis)
  (let loop ((i 0) (lis lis))
     (m:match lis
       (`((,a . ,d) . ,rest)
        (loop (+ i 1) rest))
       (`(,x . ,rest)
        (loop i rest))
       (x i))))
 
(define lis (list-tabulate 50000 (lambda (i) 
                                   (if (zero? (mod i 5))
                                       (iota (mod i 100))
                                       'x))))

(define (go)
  (time (count-pair lis))
  (time (m:count-pair lis)))
