#lang racket
(provide (contract-out (translate (-> any/c output-port? void?))))
(define (translate defs out)
  (for ((def defs))
    (translate-def def out)))

(define (translate-def def out)
  (match def
    ((list 'module m)
      (display "module " out)
      (display (symbol->string m) out)
      (display " where\n"))
  )
)
