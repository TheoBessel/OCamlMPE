

			(* ÉCHAUFFEMENT DES BLAIREAUX MASQUÉS *)

let rec puissance x n = match n with
	0 -> 1
	|n when n mod 2 = 0 -> puissance (x*x) (n/2)
	|n -> x*(puissance (x*x) (n/2));;


						(* SUJET N°1 *)

(*
Question 9 - 

On va discuter selon la position du noeud x :

	- Si x < s, le fils gauche n'est pas modifié et x qui reste inchangé 
	  minore toujours les éléments du fils droit

	- Si x = s, le fils gauche ne change pas et on ajoute 1 à x et à chaque
	  noeud du fils droit donc 1+x minore strictement le fils droit

	- Si s < x, x et le fils droit sont augmentés de 1 donc x+1 majore le 
	  fils gauche. Donc l'image de x majore celle du fils gauche et minore
	  celle du fils droit
*)

(*
Question 10 -
*)

type abr = F | N of abr * int * abr;;

let rec compte s a = match a with
	F -> 0
	|N(g,x,d) when x = s -> 1+compte s d
	|N(g,x,d) when x < s -> compte s d
	|N(g,x,d) -> compte s g+1+compte s d;;

let arbre = N(N(N(F,2,F),15,N(F,22,F)),28,N(F,41,F));;

(* # compte 20 arbre;; - : int = 3 *)

(*
Question 11 -
*)

let rec incr s a = match a with
	F -> F
	|N(g,x,d) when x = s -> N(g,x+1,incr s d)
	|N(g,x,d) when x < s -> N(g,x,incr s d)
	|N(g,x,d) -> N(incr s g,x+1,incr s d);;

(* # incr 20 arbre;; - : abr = N(N(N(F,2,F),15,N(F,23,F)),29,N(F,42,F)) *)


						(* SUJET N°2 *)

(*
1-	Une solution simple.

Q.1.

On va utiliser une fonction auxiliaire aux récursibe qui agit sur des forêts.
Elle met à jour le tableau taille des noeuds parcourus et renvoie le nombre
de ces noeuds. On fait agir aux sur [a].
*)

type arbre = N of int * arbre list;; (* N est noté Noeud dans le sujet *)

let a = N(0,[N(1,[N(2,[]);N(3,[])]);
			 N(4,[N(5,[]);N(6,[]);N(7,[N(8,[]);N(9,[])])])]);

let taille = Array.make 10 0;; (* 10 est le nombre de noeuds de a *)

(* a et taille sont deux variables globales *)

let remplir_taille () =
	let rec aux foret = match foret with
		[] -> 0
		|N(i,fils)::q -> taille.(i) <- 1+aux fils; taille.(i)+aux q
	in aux [a]; ();;

(*
Q.2.

Les éléments des sous-arbres de j sont :

	j, j+1, ..., j+taille.(j)-1

Ainsi i est l'uj des éléments si et seulement si :

	j <= i < j+taille.(j)

Ainsi on définit la fonction suivante :
*)

let appartient i j = (j <= i) && (i < j+taille.(j));;

(*
Q.3.

On cherche le plus grand entier k tel que i et j appartiennent à
un sous-arbre de k.
*)

let ppac1 i j =
	let k = ref (min i j) in
	while not((appartient i !k) && (appartient j !k))
		do k := !k-1 done;
	!k;;