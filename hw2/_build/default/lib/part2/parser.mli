type token =
  | EOF
  | FUN
  | REC
  | MATCH
  | BAR
  | END
  | GT
  | EQ
  | LT
  | LPAREN
  | RPAREN
  | DOT
  | COMMA
  | TRUE
  | FALSE
  | AND
  | OR
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | WITH
  | LAMBDA
  | NIL
  | CONS
  | TYINT
  | TYLIST
  | THINARROW
  | COLON
  | LBRACK
  | RBRACK
  | PLUS
  | SUB
  | TIMES
  | APP
  | NUMBER of (
# 18 "lib/part2/parser.mly"
        int
# 42 "lib/part2/parser.mli"
)
  | ID of (
# 19 "lib/part2/parser.mly"
        string
# 47 "lib/part2/parser.mli"
)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
