#!/bin/sh
#| -*- mode:scheme; coding:utf-8; -*-
exec sagittarius -L. -S.sld $0 "$@"
|#

(import (scheme base) (match match) (time) (srfi 1))

;; Counting pair with pttern match
(define (count-pair lis)
  (let loop ((i 0) (lis lis))
     (match lis
       (`((,a . ,d) . ,rest)
        (loop (+ i 1) rest))
       (`(,x . ,rest)
        (loop i rest))
       (x i))))
 
(define lis (list-tabulate 50000 (lambda (i) 
                                   (if (zero? (mod i 5))
                                       (iota (mod i 100))
                                       'x))))

(time (count-pair lis))
