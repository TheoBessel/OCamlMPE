type log_tree =
	Feuille of string
		|Ou of log_tree * log_tree
			|Et of log_tree * log_tree
				|Imp of log_tree * log_tree
					|Eq of log_tree * log_tree
						|Xor of log_tree * log_tree
							|Non of log_tree;;

(*
Exo 1 : Par récurrence sur n = h(a_F) :
	
	Pour F = P, h(a_F) = 0 et |F| = 1 : 3x0+1 = 1 = 2^2-3

	Supposons le résultat vrai pour h(a_F) <= n+1, n >= 1.
	Comme n >= 1, F n'est pas une variable propositionnelle :

	Donc F = (non P) ou F = (P*Q) où * est d'arité 2.

	Si F = (non P), avec a_P de hauteur n-1, alors 3h(a_P)+1 <= |p| <= 2^(h(a_P)+2)-3

	On a |F| = 3+|P| et h(a_F) = 1+h(a_P)

	Donc 3h(a_F)+1 = 3h(a_P)+3+1 <= |P|+3 = |F|

	De même : 2^(h(a_F)+2)-3 = 2x2^(h(a_P)+2)-3 >= 2^(h(a_P)+2)

	du fait que 2^(h(a_P)+2)-3 >= 0 car 2^(h(a_p)+2) >= 4

	Donc |F| = |P|+3 <= 2^(h(a_P)+2)-3

	Pour F=(P*Q) avec n-1 = max(h(a_P),h(a_Q)). On peut supposer h(a_P) >= h(a_Q)

	|F| = |P|+|Q|+3. Ses inégalités tiennent pour P et Q.

	3h(a_P)+1 <= |P| <= 2^(h(a_P)+2)-3

	3h(a_Q)+1 <= |Q| <= 2^(h(a_Q)+2)-3

	D'où 3h(a_P)+3h(a_Q)+5 <= |F| <= 2^(h(a_P)+2)+2^(h(a_Q)+2)-3

	Comme on a : 3h(a_F)+1 = 3(h(a_P)+1)+1 <= 3h(a_P)+5+3h(a_Q)

	et 2^(h(a_F)+2)-3 >= 2x2^(h(a_P)+2)-3 >= 2^(h(a_P)+2)+2^(h(a_Q)+2)-3

	Il vient 3h(a_F)+1 <= |F| <= 2^(h(a_P)+2)-3

	On a égalité pour une succession de négations ou pour un arbre saturé de connecteurs binaires
*)

type formule =
	Feuille of int
		|Ou of formule * formule
			|Et of formule * formule
				|Non of formule;;

let eval F t = 
	let rec aux g = match g with
		Feuille i -> t.(i)
			|Ou (fg,fd) -> (aux fg)||(aux fd)
				|Et (fg,fd) -> (aux fg)&&(aux fd)
					|Non (a) -> mot(aux a)
	in aux f;;

