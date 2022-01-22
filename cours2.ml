let fibo n =
	let rec aux a b k = match k with
		0 -> a
			|k -> aux b (a+b) (k-1) in aux 0 1 n;;

let rec decompose n = match n with
	0 -> []
		|k -> let rec aux a b = match (a,b) with
			(a,b) when b > k -> a
				|(a,b) -> aux b (a+b) in let p = aux 0 1 in p::decompose(k-p);;


let rec decompose_ n = let rec aux a b = match (a,b) with
		(a,b) when b > n -> a
			|(a,b) -> aux b (a+b) in match n with 
				0 -> []
					|n -> let p = aux 0 1 in p::decompose_(n-p);;

let rec ajoute n a = match a with
	Nil -> ajoute n (N(Nil,false,Nil))
		|N(g,b,d) when n = 0 -> N(g,true,d)
		|N(g,b,d) -> if n mod 2 = 0 
			then N(ajoute (n/2) g,b,d) 
				else N(g,b,ajoute (n/2) d);; (* O(log(n)) car à chaque appel récursif on diminue le nombre de bits d'une unité *)

let rec construit l = match l with
	[] -> Nil
		|(h::t) -> ajoute h (construit t);;


let rec supprime n a = match a with
	Nil -> Nil
		|N(g,b,d) -> let a = if n = 0 
			then N(g,false,d) 
				else if n mod 2 = 0 
					then N(supprime (n/2) g,b,d) 
					else N(g,b,supprime (n/2) d) 
		in if a = N(Nil,false,Nil) then Nil else a;;

let rec reunion a1 a2 = match (a1,a2) with
	(a1,Nil) -> a1
		|(Nil,a2) -> a2
			|(N(g1,b1,d1),N(g2,b2,d2)) -> N(reunion g1 g2,b1||b2,reunion d1 d2);;

let rec map f l = match l with
	[] -> []
		|h::t -> (f h)::(map f t);;

let rec elements a = match a with
	Nil -> []
		|N(g,b,d) -> let eg = map (function x -> 2*x)(elements g) in
			let ed = map (function x -> 2*x+1)(elements d) in
				if b then 0::(eg @ ed) else (eg @ ed);; (* Complexité de O(nlog(n)) !!! *)

(* On s'intéresse maintenant au parcours préfixe d'arbres *)

let res = N(N(Nil,4,N(Nil,3,Nil)),2,N(Nil,1,N(Nil,4,N(Nil,9,Nil))));;

let rec affiche a = match a with
	Nil -> ()
		|N(g,i,d) -> print_int i; affiche g; affiche d;;

let op = N(N(N(Nil,'a',Nil),'x',N(Nil,'b',Nil)),'+',N(Nil,'9',Nil));;

let rec affiche_char a = match a with
	Nil -> ()
		|N(g,i,d) -> affiche_char g; print_char i; affiche_char d;;

let rec concat l1 l2 = match l1 with
	[] -> l2
		|h::t -> h::(concat t l2);;

let rec bourrin a = match a with
	Nil -> []
		|N(g,n,d) -> n::(concat (bourrin g) (bourrin d));;

let prefixe a = 
	let rec aux b acc = match b with
		Nil -> acc
			|N(g,n,d) -> n::(aux g (aux d acc))
	in aux a [];;