#lang racket
(provide lex)
(require
  (prefix-in lex- parser-tools/lex)
  (prefix-in re- parser-tools/lex-sre)
)

(lex-define-tokens tokens (SYMBOL))
(lex-define-empty-tokens empty-tokens (LPAREN RPAREN))

(define lex
  (lex-lexer
    (lex-whitespace (lex lex-input-port))
    ((re-+ (re-or lex-alphabetic "-" "?" "!"))
     (token-SYMBOL lex-lexeme))
    ("(" (token-LPAREN))
    (")" (token-RPAREN))
  )
)

(module+ test
  (require rackunit)

  (let ((port (open-input-string "")))
    (check-equal? (lex port) 'eof)
  )

  (let ((port (open-input-string "a")))
    (check-equal? (lex port) (token-SYMBOL "a"))
    (check-equal? (lex port) 'eof)
  )

  (let ((port (open-input-string "ab")))
    (check-equal? (lex port) (token-SYMBOL "ab"))
    (check-equal? (lex port) 'eof)
  )

  (let ((port (open-input-string "ab cd")))
    (check-equal? (lex port) (token-SYMBOL "ab"))
    (check-equal? (lex port) (token-SYMBOL "cd"))
    (check-equal? (lex port) 'eof)
  )

  (let ((port (open-input-string "()")))
    (check-equal? (lex port) (token-LPAREN))
    (check-equal? (lex port) (token-RPAREN))
    (check-equal? (lex port) 'eof)
  )

  (let ((port (open-input-string "(ab cd)")))
    (check-equal? (lex port) (token-LPAREN))
    (check-equal? (lex port) (token-SYMBOL "ab"))
    (check-equal? (lex port) (token-SYMBOL "cd"))
    (check-equal? (lex port) (token-RPAREN))
    (check-equal? (lex port) 'eof)
  )
)
