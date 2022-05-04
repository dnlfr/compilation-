{
  open Parseur
  exception Eof
  exception TokenInconu
}

rule token = parse
  [' ' '\t' '\n'] { token lexbuf }
  | '/''*' ([^'*'] | '*'+ [^'*''/'])* '*''/' { token lexbuf }  
  | '+' { PLUS }
  | ['0'-'9']+ ('.' | ('.'['0'-'9']+('e''-'?['0'-'9']+)?)?) as lexem { NOMBRE(float_of_string lexem) }
  | '-' { MOINS }
  | '*' { FOIS }
  | '(' { GPAREN }
  | ')' { DPAREN }
  | '%' { MOD }
  | '?' { TERC }
  | ':' { TERS }
  | "||" { OR }
  | "&&" { AND }
  | "NaN" as lexem { NAN (lexem) }
  | "==" { EQ }
  | "<=" { GR_ST }
  | '!' { BOOL_NEG }
  | '<' { LE_ST }
  | ['a'-'z']['-' '_' '0'-'9' 'a'-'z' 'A'-'Z' ]* as lexem { VAR (lexem) }
  | '=' { AFFECT }
  | "true" | "false" as lexem {   BOOLEAN (bool_of_string lexem)}
  | eof { raise Eof }
  | ';' { PT_VIRG }
  | _ { raise TokenInconu }