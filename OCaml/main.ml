(*fichier main.ml *)
open AST

let _ =                                          (*main en OCaml*)
	if((Array.length Sys.argv) == 1)
	then
		try
	    	let lexbuf = Lexing.from_channel stdin in    (*lexeur lancé sur stdin*)
		    while true do                                (*on ne s'arrête pas*)
          Parseur.main Lexeur.token lexbuf (*parseur une ligne*)
          |> Format.printf "%a\n%!" AST.print_AST ;
		    done
	  with
	   | Lexeur.Eof          -> exit 0							 (*impossible*)
	   | Lexeur.TokenInconu                          (*erreur de lexing*)
	   | Parsing.Parse_error ->                      (*erreur de parsing*)
	       Printf.printf ("Ceci n'est pas une expression arithmetique\n")
	else
		try
			let file = Sys.argv.(1)^".jsm" in (*On recupère le nom du fichier*)
			let oc = open_out file in
			let formatter_out = Format.formatter_of_out_channel oc in
			let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in (*lexeur lancé sur fichier*)
	    while true do                                (*on ne s'arrête pas*)
        Parseur.main Lexeur.token lexbuf (*parseur une ligne*)
        |> Format.printf "%a\n%!" AST.print_AST ;
	    done
		  with
		   | Lexeur.Eof          -> exit 0							 (*impossible*)
		   | Lexeur.TokenInconu                          (*erreur de lexing*)
		   | Parsing.Parse_error ->                      (*erreur de parsing*)
		       Printf.printf ("Ceci n'est pas une expression arithmetique\n")
