#lang racket
(define str string-append)
(define jon string-join)
(define sym symbol->string)

(provide (contract-out (translate (-> any/c string?))))
(define (translate defs)
  (jon (map translate-def defs) ""))

(define (translate-def def)
  (match def
    ((list 'module m) (str "module " (sym m) " where\n"))
    ((list 'require m what)
      (str "import "
           (sym m)
           (match what
             ((list 'all) "")
             ((list-rest 'only only) (str "(" (jon (map sym only) ", ") ")"))
           )
           "\n"))
    ((list 'sig n t) (str (sym n) " :: " (translate-type t) "\n"))
    ((list 'def n v) (str (sym n) " = " (translate-value v) "\n"))
  )
)

(define (translate-type type)
  (match type
    ((list 'forall vs in)
      (str
        (if (not (null? vs))
          (str "forall " (jon (map sym vs) " ") ". ")
          "")
        (translate-type in)))
    ((list-rest '-> ts)
      (str "(" (jon (map translate-type ts) ")->(") ")"))
    ((list-rest f as)
      (str "(" (jon (map translate-type (cons f as)) ")(") ")"))
    ((? symbol? n) (sym n))
  )
)

(define (translate-value value)
  (match value
    ((list-rest f as)
      (str "(" (string-join (map translate-value (cons f as)) ")(") ")"))
    ((? symbol? n) (sym n))
  )
)
