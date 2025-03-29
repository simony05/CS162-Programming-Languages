
(* The type of tokens. *)

type token = 
  | WITH
  | VOID
  | TYLIST
  | TYINT
  | TYBOOL
  | TRUE
  | TIMES
  | TICK
  | THINARROW
  | THEN
  | SUB
  | SND
  | RPAREN
  | REC
  | RBRACK
  | PLUS
  | NUMBER of (int)
  | NIL
  | MATCH
  | LT
  | LPAREN
  | LET
  | LBRACK
  | LAMBDA
  | IS
  | IN
  | IF
  | ID of (string)
  | GT
  | FUN
  | FST
  | FORALL
  | FIX
  | FILE of (string)
  | FALSE
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | CSYNTH
  | CSAVE
  | CPRINT
  | CONS
  | COMMA
  | COLON
  | CLOAD
  | CLET
  | CCLEAR
  | CASE2
  | CASE1
  | BAR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val ty_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.ty)

val script_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Cmd.t list)

val repl_command_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Cmd.t)

val expr_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
