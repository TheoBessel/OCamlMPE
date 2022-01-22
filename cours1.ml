type 'a abf = Nil | F of 'a | N of 'a abf * 'a abf;;

let rec decode_info a l = match (a,l) with
	Nil,_ -> failwith "erreur"
		| F x, [] -> x
			| _, [] -> failwith "erreur"
				| F x, _ -> failwith "erreur"
					| N(g,d), true::q -> decode_info g q
						| N(g,d), false::q -> decode_info d q;;

let a = N(N(F('a'),F('b')),F('c'));;

let l = [true;false];; (*return b*)

decode_info a l;;

let rec code_info a x = match a with
	Nil -> failwith "pas de code"
		| F y -> if x=y then [] else failwith "pas de code"
			| N(g,d) -> try true::(code_info g x) with Failure "pas de code" -> false::(code_info d x);;

let rec basep m p = match m with
	m when m < p -> string_of_int(m)
		|m -> basep (m/p) p ^ basep (m mod p) p;;

type 'a arbre = Nil | N of 'a arbre * 'a * 'a arbre;;

let test = N(N(Nil,false,N(Nil,true,Nil)),false,N(Nil,false,N(Nil,false,N(Nil,true,Nil))));; (* represents {2,7} *)

let rec verifie a = match a with
	Nil -> true
		| N(Nil,b,Nil) -> b
			| N(g,_,d) -> (verifie g)&&(verifie d);;

verifie test;; (* return true :) *)

let rec cherche n a = match a with
	Nil -> false
		| N(_,b,_) when n = 0 -> b
			| N(g,_,d) -> if n mod 2 = 0 then cherche (n/2) g else cherche (n/2) d;;

cherche 2 test;; (* true *)
cherche 7 test;; (* true *)
cherche 5 test;; (* false *)