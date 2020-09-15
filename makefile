default : run	
run :
	@ocamlc -c logpro.ml
	@ocamllex tokenizer.mll
	@ocamlyacc parser.mly     
	@ocamlc -c  parser.mli
	@ocamlc -c  tokenizer.ml
	@ocamlc -c  parser.ml
	@ocamllex lexquery.mll
	@ocamlyacc parsequery.mly     
	@ocamlc -c  parsequery.mli
	@ocamlc -c  lexquery.ml
	@ocamlc -c  parsequery.ml
	@ocamlc -c main.ml
	@ocamlc -o assignment6 tokenizer.cmo logpro.cmo parser.cmo lexquery.cmo parsequery.cmo main.cmo 	
runwarn: 
	@ocamlc -c -w -1..27 logpro.ml
	@ocamllex tokenizer.mll
	@ocamlyacc parser.mly     
	@ocamlc -c -w -1..27 parser.mli
	@ocamlc -c -w -1..27 tokenizer.ml
	@ocamlc -c -w -1..27 parser.ml
	@ocamllex lexquery.mll
	@ocamlyacc parsequery.mly     
	@ocamlc -c -w -1..27 parsequery.mli
	@ocamlc -c -w -1..27 lexquery.ml
	@ocamlc -c -w -1..27 parsequery.ml
	@ocamlc -c -w -1..27 main.ml
	@ocamlc -o assignment6 tokenizer.cmo logpro.cmo parser.cmo lexquery.cmo parsequery.cmo main.cmo 	