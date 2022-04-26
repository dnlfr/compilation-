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
  | eof { raise Eof }
  | _ { raise TokenInconu }