#lang racket

(define (f x)
  (* x x))

(module+ test
  (require rackunit)

  (check-equal? 1 2))
