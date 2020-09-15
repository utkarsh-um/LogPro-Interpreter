open Printf
open Logpro
let parser_work =
			let in_stream = open_in (Sys.argv.(1)) in
          try
            let lexbuf = Lexing.from_channel in_stream in
            let cmnds=[] in
            while true do
              let result = Parser.line Tokenizer.token lexbuf in
              let a=result in
                flush stdout
            done
          with End_of_file -> close_in in_stream;;(*  exit 0;; *)
            

let parser_work2 = (* function *)
		printf "\nEnter Queries\n?-"; flush stdout;
          try
          	
            let lexbuf = Lexing.from_channel stdin in
            let cmnds=[] in
            while true do

              let result = Parsequery.line Lexquery.token lexbuf in
              let a=result in
                flush stdout
            done
          with End_of_file -> exit 0;;