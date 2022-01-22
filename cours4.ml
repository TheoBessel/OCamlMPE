let rec pgcd p q = match q with 
		0 -> p
			|q -> pgcd q (p mod q);;

let phi n = 
	let rec help n k = match k with
		1 -> 1
			|k -> if pgcd k n = 1 then 1 + help n (k-1) else help n (k-1)
	in help n n;;

(*
Exo 9 : On va utiliser la fonction taille afin d'initialiser un tableau pere de la bonne taille.
		On peut introduire un programme auxilière recursif

			aux : int -> (int arbregen) list -> unit

		qui va mettre à jour pere.
*)

let arbretoarray (M(i,f)) = 
	let n = taille (M(i,f)) in
		let pere = Array.make n i in
			let rec aux k fk = match fk with
				[] -> ()
					|(M(j,fj))::q -> pere.(j) <- k;
						aux j fj; aux k q
			in aux i f; pere;;

let a = M(0,[M(1,[M(4,[]);M(5,[])]);M(2,[]);M(3,[M(6,[])])]);;

arbretoarray a;; (* - : int array = [|0; 0; 0; 0; 1; 1; 3|] 
						(pour la racine on a utilisé la convention pere.(i) = i ) *)

let peretofils pere = 
	let n = Array.length pere in
		let fils = Array.make n [] in
			let r = ref (-1) in (* sert à stocker la racine *)
				for i=0 to (n-1) do
					let j = pere.(i) in
						if j <> i then
							fils.(j) <- i::fils.(j)
						else r := i done;
	!r, fils;;

(*
On peut utiliser une sous-procédure récursive

	aux : int -> int list -> (int arbregen)

telle que l'appel aux i (fils i) renvoie le sous arbre associé à (fils i)
*)

let arraytoarbre pere =
	let r, fils = peretofils pere in
		let rec aux k fk = match fk with
			[] -> M(k,[])
				|i::q -> let (M(x,l)) = aux k q
		in M(k,(aux i fils.(i))::l)
	in aux r fils.(r);;

let vectoarbre arr =
	let (root,fils) = peretofils arr in
		let rec aux i = M(i,map aux (fils.(i)))
	in aux root;;

arraytoarbre (arbretoarray a);; (* M(0,[M(3,[M(6,[])]);M(2,[]);M(1,[M(5,[]);M(4,[])])]) 
									-> (OK, on a juste permuté les fils !) *)

(*
Exo 10 : Si a est un arbre binaire :

	(a est un ABR) <=> (la liste formée à partir du parcours infixe de a est 
												strictement croissante)

	=> Par recurrence sur |a| :
		-> le résultat est clair pour |a| <= 1
		-> supposons que a = N(g,x,d); on sait que g et d sont des ABR 
			donc l'hypothèse de récurrence assure que l_g et l_d,
			issues des parcours infixes respectifs de g et d, 
		 	sont triées avec leurs éléments respectifs inférieurs puis supérieurs à x.

		 	Comme on sait l_a, la liste issue du parcours infixe de a s'écrit l_g @ [x] & l_d,
		 	d'où on en déduit le résultat par récurrence.

	<= Encore par récurrence sur |a| :
		-> Si l_a est croissante, on a clairement l_g (resp. l_d) classées par ordre croissant
			constituées d'éléments <= x (resp. >= x) donc par hypothèse de récurrence,
			g et d sont des ABR avec max g <= x <= min d donc a est bien un ABR.
*)

