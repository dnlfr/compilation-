type token =
  | NOMBRE of (int)
  | BOOLEAN of (bool)
  | PLUS
  | MOINS
  | FOIS
  | GPAREN
  | DPAREN
  | EOL
  | MOD
  | GR_ST
  | LE_ST
  | EQ

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AST.expression_a
