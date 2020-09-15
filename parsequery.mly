%{
	open Printf
	open Logpro	

	let parse_error s = 
  	print_endline ;
  	flush stdout
  	let rec print_list = function 
	[] -> ()
	| e::l -> print_int e ; print_string " " ; print_list l
%}

%token <string> CONST VAR 
%token <char> LPARN RPARN CONJ CLT NL
%token  EOF
%start line
%type <unit> line program
%type <Logpro.term> term
%type <(Logpro.term) list> teli
%type <Logpro.atom> atom
%type <(Logpro.atom) list> atli
%%
line:	{}
	|line program {}
	|line program EOF {raise End_of_file}
	;

program:atli CLT		{Logpro.findans $1}
	;

atli:atom {[$1]}
	|atli CONJ atom {$1@[$3]}
	;

atom:CONST LPARN teli RPARN {($1,$3)}
	;

teli:term {[$1]}
	|teli CONJ term	 {$1@[$3]}
	;

term:VAR {Logpro.V($1)}
	|CONST {Logpro.C($1)}
	|CONST LPARN teli RPARN {Logpro.F($1,$3)}
	;


/* sheet is printed after every instruction */