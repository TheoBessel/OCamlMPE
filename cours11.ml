let rec distance x l = match l with
	[e] -> max (x-e) (e-x)
	|h::t -> let d = distance x t in 
		if d < (max (x-h) (h-x))
			then d
		else max (x-h) (h-x);;

(*
Exo 5 : on utilise le couple i,j : 
	
	i = bit de parité de la longueur

	j = congruence modulo 3 du nombre de a
*)

type local = { v : bool; p : char list ; s : char list ; f : (char * char) list};;

let rec appartient x l = match l with
	[] -> false
	|h::t -> h = x || (appartient x t);;

let est_dans l m =
	let n = String.length m in
		if n = 0 then l.v (* cas du mot vide *)
		else if not(appartient m.[0] l.p) then false
		else begin
			let i = ref 0 in  (* on utilise les facteurs de longueur 2 *)
			while (!i < n-1) && (appartient (m.[!i],m.[!i+1]) l.f) do
				incr i done;
			(!i=n-1) && (appartient m.[!i] l.s)
		end;;

(*
Exo 8 - 9 : 

	Soit A = {a,b} l'alphabet

	-> L1 = ab et L2 = ba sont clairement locaux.
		-> L = L1+L2 = {ab,ba}
		-> P(L) = {a,b} = S(L)
		-> F(L) = {ab,ba}
	Cependant, aba n'appartient pas à L et vérifie les critères donc L n'est pas local

	De même : 
		-> L' = L1^2 = L1.L1 = {abab}
		-> P(L') = a
		-> S(L') = b
		-> F(L') = {ab,ba}
	Cependant, ab appartient à L'
*)

(*
Exo 10 :

	L = ( ( X+ 3* 1* X* )* + ( X* 3* 1* X+ )* )*
	
	L = (X + 3)* ( 1+ X(X+3)* )* 1*

	L = 3* ( (X+1)*X 3+ )* (X + 3)*
*)