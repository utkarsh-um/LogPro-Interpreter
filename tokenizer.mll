{
	open Parser
	
	open Printf


}
rule token = parse
	 |['a'-'z']['a'-'z''A'-'Z']* as fcons
		{
			CONST fcons
		}
	|['A'-'Z']['a'-'z''A'-'Z']* as vars
		{
			VAR vars
		}
	|'(' as lp
		{
			
			LPARN lp
						
		}

	|')' as rp
		{
			
			RPARN rp
			
		}

	|',' as cm
		{
			 
			CONJ cm
			
		}

	|":-" as cl
		{
			
			COND cl
			
		}

	|'.' as fort
		{
			CLT fort
			
		}
	|['\n' ' ' '\t']+	{token lexbuf}
	|_ as c 	{printf "lexical error"; exit 0;token lexbuf}
	| eof 	 {EOF}











