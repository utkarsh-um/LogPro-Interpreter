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

%token <string> CONST COND VAR 
%token <char> LPARN RPARN CONJ CLT NL
%token  EOF
%start line
%type <unit> line
%type <Logpro.clause> clause
%type <Logpro.clause list> program
%type <Logpro.term> term
%type <(Logpro.term) list> teli
%type <Logpro.atom> atom
%type <(Logpro.atom) list> atli
%type <Logpro.fact> fact
%type <Logpro.rule> rule
%%
line:	{}
	|program EOF { let s1 = (Logpro.createBase $1 0 0) in raise End_of_file}
	;

program:clause 			{[$1]}
	| program clause	{$1@[$2]}
	;


clause:fact CLT {Logpro.Fc($1)}
	|rule CLT 	{Logpro.Rl($1)}
	;

fact:atom {$1}
	;

rule:atom COND atom {($1,[$3])}
	|atom COND atli {($1,$3)}
	;

atli:atom CONJ atom {[$1;$3]}
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