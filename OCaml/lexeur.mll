{
  open Parseur
  exception Eof
  exception TokenInconu
}

rule token = parse
  [' ' '\t' '\n'] { token lexbuf }
  | '/''*' ([^'*'] | '*'+ [^'*''/'])* '*''/' { token lexbuf }  
  | "==" { EQ }
  | '=' { AFFECT }
  | ['0'-'9']+ ('.' | ('.'['0'-'9']+('e''-'?['0'-'9']+)?)?) as lexem { NOMBRE(float_of_string lexem) }
  | "if" { IF }
  | "else" { ELSE }
  | "do" { DO }
  | "while" { WHILE }
  | "for" { FOR }
  | "NaN" as lexem { NAN (lexem) }
  | "true" | "false" as lexem {   BOOLEAN (bool_of_string lexem)}
  | ['a'-'z']['-' '_' '0'-'9' 'a'-'z' 'A'-'Z' ]* as lexem { VAR (lexem) }
  | '+' { PLUS }
  | '-' { MOINS }
  | '*' { FOIS }
  | '(' { GPAREN }
  | ')' { DPAREN }
  | '}' { DBRACKET }
  | '{' { GBRACKET }
  | '%' { MOD }
  | '?' { TERC }
  | ':' { TERS }
  | "||" { OR }
  | "&&" { AND }
  | "<=" { GR_ST }
  | '!' { BOOL_NEG }
  | '<' { LE_ST }
  | eof { raise Eof }
  | ';' { PT_VIRG }
  | _ { raise TokenInconu }