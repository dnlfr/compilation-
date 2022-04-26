{
  open Parseur
  exception Eof
  exception TokenInconu
}

rule token = parse
  [' ' '\t' '\n'] { token lexbuf } 
  | '+' { PLUS }
  | ['0'-'9']+ ('.' | ('.'['0'-'9']+('e''-'?['0'-'9']+)?)?) as lexem { NOMBRE(float_of_string lexem) }
  | '-' { MOINS }
  | '*' { FOIS }
  | '(' { GPAREN }
  | ')' { DPAREN }
  | '%' { MOD }
  | "==" { EQUAL }
  | "<=" { GR_ST }
  | '!' { BOOL_NEG }
  | '<' { LE_ST }
  | "true" | "false" as lexem    									                   {   BOOLEAN (bool_of_string lexem)}
  | eof { raise Eof }
  | _ { raise TokenInconu }