{
  open Parseur
  exception Eof
  exception TokenInconu
}

rule token = parse
  [' ' '\t' '\n'] { token lexbuf } 
  | ['0'-'9']+ ('.' | ('.'['0'-'9']+('e''-'?['0'-'9']+)?)?) as lexem { NOMBRE(int_of_string lexem) }
  | '+' { PLUS }
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