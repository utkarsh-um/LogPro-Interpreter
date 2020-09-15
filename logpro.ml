
open Printf
module Logpro =
   struct
   exception NOT_UNIFIABLE;;
   	type term = C of string|V of string|F of string*(term list);;
   	type atom = string*(term list);;
   	type fact = atom;;
   	type rule = atom*(atom list);;
   	type clause = Fc of fact|Rl of rule;;
   	type pseudocl = Cl of clause|EMPTY;;

   	let (knowledge:pseudocl array) = Array.make 5000 (EMPTY);;

(*-------------------------------------------------------*)
   	let rec map l f = match l with
	[]->[]
	| x::xs -> (f x)::(map xs f);;

	let rec foldl f e l = match l with
		[] -> e
	| x::xs -> foldl f (f x e) xs;;
(*-------------------------------------------------------*)
let rec addvars l x = match l with
		[]-> x::[]
	| f::fs -> if (String.equal f x) then f::fs else f::(addvars fs x);;

let rec vars_help (t:term) (l:string list)= match t with 
	V(x) -> addvars l x
	| C(x) -> l
	|  F(s,tl) ->  let f xt e = vars_help xt e in
					foldl f l tl;;

	let vars (t:term) = vars_help t [];; (*return a list of variables in term t*)



	let rec reverse v rev = match v with
					[]->rev
					| x::xs -> (reverse xs (x::rev));;
(*-------------------------------------------------------*)

type substitution = (string*term)list;;

let compose (s1:substitution) (s2:substitution) : substitution= s1@s2;;

let rec subst_helper (sub:(string*term)) (t:term) = match t with  
					| V(x) -> let (v1,t1) = sub in if (String.equal v1 x) then t1 else t
					| C(x) -> t
					| F(s,tl) -> let f xt = subst_helper sub xt in
							F(s,(map tl f));;

let rec subst  (sublis:(substitution)) (t:term) = match sublis with  (*apply substitution to a term*)
					| [] -> t
					| sx::subxs -> subst  subxs (subst_helper  sx t);;

let rec atsub (sublis) (a : atom) = let (s,tl) = a in 		(*apply substitution to an atom*)
					let f xt = subst sublis xt in
						(s,(map tl f));;

let rec substalist (sublis) (ali : atom list)= match ali with (*apply substitution to a list of atoms*)
					[] -> []
					| x::xs  -> (atsub sublis x)::(substalist sublis xs);;

(*-------------------------------------------------------*)


let rec change_variables (vl:string list) l (e:(string*term) list) =
	 match vl with
	[] -> (e,l)
	| x::xs -> let ad = (String.concat "" ["_U";string_of_int(l)]) in change_variables xs (l+1) ((x,V(ad))::e);;


let rec vars_Fact tl l = 
   	match tl with
   	[] -> l
   	| t::ts -> vars_Fact ts (vars_help t l);;


let rec rectifyFact x l = let (z,tl) = x in
   						let vlf = vars_Fact tl [] in
   						let (subli,j) = change_variables vlf l [] in
   						((atsub subli x),j);;

let rec vars_Rule (atli:atom list) (li) = 
   	match atli with
   	[] -> li
   	|x::xs -> let (z,tl) = x in vars_Rule xs (vars_Fact tl li);;


let rectifyRule (x:rule) l : (rule*int)= 
   								let (at,atli) = x in
   								let (z,tl) = at in
   								let vlf = vars_Fact tl [] in
   								let vrf = vars_Rule atli vlf in 
   								let (subli,j) = change_variables vrf l [] in 
   								(((atsub subli at),(substalist subli atli)),j);;

let rec rectify (x:clause) l = match x with 			
   	Fc(x1) -> let (z,j) = (rectifyFact x1 l) in (Fc(z),j)
   	| Rl(x1) -> let (z,j) = (rectifyRule x1 l) in (Rl(z),j);;


let rec createBase (base:clause list) (i) l= match base with 		(*stores all the facts and rules in an array of size 5000 named knowledge, with no two different clauses having same variable names*)
   	[] -> ()
   	| x::xs ->let (z,d)=rectify x l in let ik = knowledge.(i) <- Cl(z) in createBase xs (i+1) d;;

(*-------------------------------------------------------*)


let rec print_termList tl = match tl with 
   	[] -> ()
   	| x::[] -> print_Term x ;
   	| x::y::xs -> print_Term x ; printf ","; print_termList (y::xs);

and print_Term x = match x with 
   	V(x) -> printf "V(%s)" x 
   	| C(x) -> printf "%s" x 
   	| F(s,tl) -> printf "%s(" s; print_termList tl; printf ")";;

let rec print_list li = match li with
	[] -> ()
	| (x,t)::[] ->  printf "%s= " x ; print_Term t
	| (x,t)::y::l ->  printf "%s= " x ; print_Term t; printf " , " ; print_list (y::l);;

let rec print_list1 li=match li with
	[] -> ()
	| e::[] ->let ad = print_list e in print_string "\n" 
	| e::l -> let ad = print_list e in let ag = print_string "  " in  let af = (read_line ()) in if (String.equal af ";") then (print_list1 l)
											else printf "\n";;

(*-------------------------------------------------------*)

let rec check_vars (l:string list) (x:string) = match l with  			
| [] -> false
| f::xs -> if (String.equal f x) then true else  (check_vars xs x);;



let rec mgu (t1:term) (t2:term) = match (t1,t2) with 		(*returns the mgu of two terms*)
		(C(x),C(y)) -> if (String.equal x y) then [] else raise NOT_UNIFIABLE
		| (V(x),V(y)) -> if (String.equal x y) then [] else [(y,V(x))] 
		| (C(x),V(y)) -> [(y,t1)]
		| (V(x),C(y)) -> [(x,t2)]
		| (C(x),F(_,_)) -> raise NOT_UNIFIABLE
		| (F(_,_),C(x)) -> raise NOT_UNIFIABLE
		| (V(x),F(s,tl)) -> let ar = (List.length tl) in 				
							let checker = check_vars (vars  t2) x in
							if checker then raise NOT_UNIFIABLE
							else [(x,t2)]

		| (F(s,tl),V(x)) -> let ar = (List.length tl) in
							let checker = check_vars (vars  t1) x in
							if checker then raise NOT_UNIFIABLE
							else [(x,t1)]

		|(F(s1,tl1),F(s2,tl2)) -> if(String.equal s1 s2) then  			
								let ar1=(List.length tl1) in
								let ar2 = (List.length tl2) in
								if ar1 = ar2 then operate_list  tl1 tl2 []
									else raise NOT_UNIFIABLE
							else raise NOT_UNIFIABLE

and operate_list (tlis1: (term list)) (tlis2: (term list)) l = match (tlis1,tlis2) with
		| ([],[]) -> l
		| ([],x::xs) -> raise NOT_UNIFIABLE
		| (x::xs,[]) -> raise NOT_UNIFIABLE
		| (tis1::ts1,tis2::ts2) -> operate_list  ts1 ts2 (compose l (mgu  (subst  l tis1) (subst  l tis2)))

and findAtom (i:int) (query:atom) (l:substitution list) (chk:bool) : (substitution list*bool)= let (at,atli) = query in  (*returns a list of substitutions/answers to an atom/query*)
	match knowledge.(i) with
		EMPTY -> (l,chk)
		| Cl(x) -> match x with 
			Fc(x1) -> let (s,tl) = x1 in if(String.equal s at) then try let s1 = operate_list tl atli [] in findAtom (i+1) query (s1::l) true 
																with NOT_UNIFIABLE -> findAtom (i+1) query l chk
										else findAtom (i+1) query l chk

			| Rl(x1) -> let ((s,tl),al) = x1 in if(String.equal s at) then 
													let s1 = operate_list tl atli [] in 
													let (calie,ctu) = (backAtom ([(s1,al)]) [] false) in
													match  calie with
														[] -> findAtom (i+1) query l chk 
														| gh::ghs -> findAtom (i+1) query (calie@l) true 
												else findAtom (i+1) query l chk

and backAtom (l:(substitution*(atom list)) list) (e:(substitution list)) (chk:bool) : (substitution list*bool)= match l with (*backtracking appied on the list of atoms to find simultaneous solutions*)
		[] -> (e,chk)
		| x::xs -> let (a,b) = x in 
			match b with 
				[] -> backAtom xs (a::e) true
				| f::fs -> let (g,bo) = (findAtom 0 (atsub a f) [] false) in
							if bo then 
								let funt xt = ((compose a xt),fs) in
								backAtom ((map g funt)@xs) e chk
							else backAtom xs e chk;;

(*-------------------------------------------------------*)

let rec get_subst sub x l = match sub with 
[] ->(x,V(x))
| (y,t)::ys -> if(String.equal x y) then (y,(subst ((reverse l [])@ys) t))
				else get_subst ys x ((y,t)::l);;

let rec process_each (sub:substitution) varli l= match varli with 
[] -> reverse l []
| x::xs -> let sub1 = get_subst sub x [] in process_each sub xs (sub1::l);;

let rec process_ans (sublis:substitution list) (varli:string list) (l:substitution list) = match sublis with 
[] -> l 
|sub::ts  -> process_ans ts varli ((process_each sub varli [])::l);;

let  findans qu =
 let ad = vars_Rule qu [] in
			let (af,an) = (backAtom [([],qu)] [] false) in
			match ad with
				[] -> printf "%B\n \n?-" an; flush stdout
				| x::xs -> let x1 = process_ans (af) (ad) []  in 
					match x1 with 
						[] -> printf "false\n \n?-"; flush stdout
						|x::xs -> printf "true\n"; print_list1 x1; printf "\n?-" ;flush stdout; 

(*-------------------------------------------------------*)



end;;


