(* Échauffement *)

let rec decompose n = match n with
	1 -> []
	|n -> let i = ref 2 in
		while !i <= n && (n mod !i <> 0) do
			incr i;
		done;
		let p = ref !i in
		let k = ref 0 in
		while n mod !p = 0 do
			incr k;
			p := !p*(!i)
		done;
		(!i,!k)::decompose((n*(!i))/(!p));;

(*
Exo 1 :

a)
Chaque arrête admet deux extremités distinctes et donc chacune d'elle est compté deux fois 
lorsqu'on somme les degrés des sommets.

On obtient donc : Somme sur s dans S des `degré(s)` = 2 * `nombre d'arêtes`

b)
Introduisons le graphe dont les sommets sont ces joueurs (ce qui fait 5 sommets)
et dont les arêtes sont les parties (ce qui fait 15 arrêtes) qui est impair.

Ce qui précède montre l'imposibilité de l'existence d'un tel graphe
*)

(*
Exo 2 :

Introduisons le graphe qui a pour sommets les n personnes et pour arêtes les paires de personnes qui se connaissent.
Supposons le résultat faux.

Chaque sommet a un degré distinct donc les degrés parcourent exactement les entiers de 0 à n-1.

Ainsi il existe une personne qui connaît tout le monde (sommet de degré n-1)
et une autre qui ne connaît personne (sommet de degré 0).

Cela est absurde ! Donc le résultat est vrai.
*)

type graphe = int array array;;

let deg_sortant i g =
	let d = ref 0 in
		for k = 0 to Array.length(g)-1 do
			if g.(i).(k) then incr d
		done;
	!d;;

let deg_entrant i g =
	let d = ref 0 in
		for k = 0 to Array.length(g)-1 do
			if g.(k).(i) then incr d
		done;
	!d;;

(*
Exo 3 :

[0,1,0,0,0,0,0]
[0,0,1,0,0,0,0]
[0,0,0,1,0,0,1]
[0,0,0,0,1,0,0]
[1,0,0,0,0,1,0]
[0,0,0,1,0,0,1]
[0,1,0,0,0,0,0]
*)

let g = [|
	[|0;1;0;0;0;0;0|];
	[|0;0;1;0;0;0;0|];
	[|0;0;0;1;0;0;1|];
	[|0;0;0;0;1;0;0|];
	[|1;0;0;0;0;1;0|];
	[|0;0;0;1;0;0;1|];
	[|0;1;0;0;0;0;0|]
|];;

(*
Exo 4 :
*)

let desoriente g =
	let n = Array.length(g) in
	for i = 0 to (n-1) do
		for j = i+1 to (n-1) do
			g.(i).(j) <- g.(i).(j) lor g.(j).(i);
			g.(j).(i) <- g.(i).(j)
		done;
	done;;

(*
Pour l'opération inverse on ne fait rien.
*)

(*
Exo 5 :

Notons S = {1,...,n} l'ensemble des sommets.

Montrons par récurrence :

H_m : a_{ij}^{(m)} = 1 => (il existe un chemin de longueur m qui joint i à j)


	- On a H_1 vérifié du fait de la définition de la matrice d'adjacence.


	- Supposons le résultat vrai jusqu'à m.

		Soient i,j tels que a_{ij}^{(m+1)} = 1. Il existe un sommet k tel que a_{ik} = 1 et a_{kj}^{(m)} = 1.

		L'hypothèse de récurrence assume l'existence d'un chemin de longueur m 

							[k_1=k,k2,...,k_m=j]

		qui joint k à j.

		Par ailleurs, il existe une arête entre i et k.

		Alors [k_0=i,k_1=k,...,k_m=j] constitue un chemin de longueur m+1 qui joint i à j.

De même, réciproquement, on pose :

H'_m : a_{ij}^{(m)} = 1 <= (il existe un chemin de longueur m qui joint i à j)

	- H'_1 est clair

	- Supposons le résultat vrai jusqu'à m

		Soient i,j tels qu'il existe un chemin de longueur m+1 joignant i à j :

							[k_0=i,k_1=k,...,k_m=j]

		L'hypothèse de récurrence assure que a_{k_1 j}^{(m)}=1 et que a_{i k_1} = 1

		D'où a_{ij}^{(m+1)} = 1 et on a le résultat.

D'où l'équivalence.
*)

type graphe_liste = (int list) array;;

let degre_sortant i g =
	List.length g.(i);;

let degre_entrant i g =
	let r = ref 0 in
	for k = 0 to (Array.length(g) - 1) do
		if List.mem i g.(k)
			then incr r
	done;
	!r;;

(*
Exo 6 :

On trouve [|[1]; [2]; [3;6]; [4]; [5;0]; [3;6]; [1]|]
*)

(*
Exo 7 :

Pour chaque i et chaque j dans g.(i), on ajoute i à la liste des prédécesseurs de j.

On créée un tableau de listes de prédecesseurs que l'on met à jour.

On utilise un programme récursif auxiliaire qui va agir sur la liste des successeurs d'un sommet fixé.
*)

let predecesseur g =
	let n = Array.length g in
	let pred = Array.make n [] in
	let rec aux l i = match l with
		[] -> ()
		|j::q -> pred.(j) <- i::pred.(j);
	aux q i in
	for i = 0 to (n-1) do
		aux g.(i) i
	done;
	pred;;

(*
Notons n le nombre de sommets et m le nombre d'arêtes :


La création de pred a une complexité en O(n).

Les instructions de la boucle d'indice i ont une complexité de |g(l)|

La complexité issue des boucles est en O(m).


En résumé la complexité est en O(n+m)
*)

(*
Exo 8 :
*)

let adg_to_list g =
	let n = Array.length g in
	let t = Array.make n [] in
		for i = 0 to (n-1) do
			for j = 0 to (n-1) do
				if g.(i).(j) then t.(i) <- j::t.(i)
			done;
		done;
	t;;