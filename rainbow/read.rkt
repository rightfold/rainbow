#lang racket
(provide lex)
(require
  (prefix-in lex- parser-tools/lex)
  (prefix-in re- parser-tools/lex-sre)
)

(lex-define-tokens tokens (SYMBOL))
(lex-define-empty-tokens empty-tokens (LPAREN RPAREN LBRACKET RBRACKET))

(define lex
  (lex-lexer
    (lex-whitespace (lex lex-input-port))
    ((re-+ (re-or lex-alphabetic "-" "?" "!"))
     (token-SYMBOL lex-lexeme))
    ("(" (token-LPAREN))
    (")" (token-RPAREN))
    ("[" (token-LBRACKET))
    ("]" (token-RBRACKET))
  )
)

(module+ test
  (require rackunit)

  (define (test-lex text tokens)
    (let ((port (open-input-string text)))
      (for ((token tokens))
        (check-equal? (lex port) token)
      )
      (check-equal? (lex port) 'eof)
    )
  )

  (test-lex "" (list))
  (test-lex "a" (list (token-SYMBOL "a")))
  (test-lex "ab" (list (token-SYMBOL "ab")))
  (test-lex "ab cd" (list (token-SYMBOL "ab") (token-SYMBOL "cd")))
  (test-lex "()" (list (token-LPAREN) (token-RPAREN)))
  (test-lex "(ab)" (list (token-LPAREN) (token-SYMBOL "ab") (token-RPAREN)))
  (test-lex "[ab]" (list (token-LBRACKET) (token-SYMBOL "ab") (token-RBRACKET)))
)
