#lang racket
(provide
  (contract-out
    (read (-> string? list?))))
(require
  (prefix-in lex- parser-tools/lex)
  (prefix-in re- parser-tools/lex-sre)
  (prefix-in yacc- parser-tools/yacc)
)

(lex-define-tokens tokens (SYMBOL))
(lex-define-empty-tokens empty-tokens
  (EOF LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE))

(define lex
  (lex-lexer
    ((re-or lex-whitespace ",") (lex lex-input-port))
    ((re-+ (re-or lex-alphabetic "-" "?" "!"))
     (token-SYMBOL (string->symbol lex-lexeme)))
    ("(" (token-LPAREN))
    (")" (token-RPAREN))
    ("[" (token-LBRACKET))
    ("]" (token-RBRACKET))
    ("{" (token-LBRACE))
    ("}" (token-RBRACE))
    ((eof) (token-EOF))
  )
)

(define parse
  (yacc-parser
    (start sexprs)
    (end EOF)
    (error void)
    (tokens tokens empty-tokens)
    (grammar
      (sexprs
        (()             '())
        ((sexpr sexprs) (cons $1 $2))
      )
      (sexpr
        ((SYMBOL) $1)
        ((LPAREN sexprs RPAREN) $2)
        ((LBRACKET sexprs RBRACKET) (list->vector $2))
        ((LBRACE sexprs RBRACE) (apply hash $2))
      )
    )
  )
)

(define (read text)
  (let ((port (open-input-string text)))
    (parse (lambda () (lex port)))))

(module+ test
  (require rackunit)

  (define (test-lex text tokens)
    (let ((port (open-input-string text)))
      (for ((token tokens))
        (check-equal? (lex port) token)
      )
      (check-equal? (lex port) (token-EOF))
    )
  )

  (test-lex "" (list))
  (test-lex " \t\r\n," (list))
  (test-lex "a" (list (token-SYMBOL 'a)))
  (test-lex "ab" (list (token-SYMBOL 'ab)))
  (test-lex "ab cd" (list (token-SYMBOL 'ab) (token-SYMBOL 'cd)))
  (test-lex "()" (list (token-LPAREN) (token-RPAREN)))
  (test-lex "(ab)" (list (token-LPAREN) (token-SYMBOL 'ab) (token-RPAREN)))
  (test-lex "[ab]" (list (token-LBRACKET) (token-SYMBOL 'ab) (token-RBRACKET)))
  (test-lex "{ab}" (list (token-LBRACE) (token-SYMBOL 'ab) (token-RBRACE)))

  (define (test-parse text sexprs)
    (check-equal? (read text) sexprs))

  (test-parse "" '())
  (test-parse "a" '(a))
  (test-parse "a b" '(a b))
  (test-parse "a () (b)" '(a () (b)))
  (test-parse "a [] [b]" '(a #[] #[b]))
  (test-parse "a {} {b c d e}" (list 'a (hash) (hash 'b 'c 'd 'e)))
)
