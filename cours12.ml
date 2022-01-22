let affiche n =
	let rec aux k = match k with
		0 -> [n]
		|k -> (n-k)::(aux (k-1))
	in aux n;;

let affiche n =
	let rec aux acc = function
		|k when k<0 -> acc
		|k -> aux (k::acc) (k-1)
	in aux [] n;;



(*
Exo 5 :

tu = vw <=> ((t=vz et zu = w) ou (v=tz et zw=u)).

Supposons |t| <= |v| pour se fixer les idées, tu=vw conduit à "t est un préfixe de v"

Donc il existe z dans A* tel que v=tz.

On en déduit tu=vw <=> tu=tzw <=> u=zw.
*)

(*
Exo 6 :

Soient u,v dans A* tels que uv=vu, montrons qu'il existe w tel que u=w^p et v=w^q, avec p,q dans IN.

La condition est clairement suffisante. On a clairement uv=vu=w^(p+q).

Réciproquement, supposons uv=vu.
On raisonne par récurrence sur la longueur de |u|+|v| :
	- Si |u|+|v|=0 on a u=v=eps, le résultat est acquis.
	- Supposons le résultat acquis pour |u|+|v|<=n-1 avec n>= 1
		Considérons u,v tels que |u|+|v|=n et uv=vu, supposons |u|<=|v| pour se fixer les idées.
		Ce qui précède assure l'existence de z dans A* tel que v=uz
		Il vient alors uz=zu.
			-> Si u=eps, le résultat est acquis.
			-> Sinon |z|<|v| et donc |u|+|z|<|u|+|v|, on peut appliquer l'hypothèse de récurrence :
				Il existe w dans A* tel que u=w^p, z=w^(q1).
				Il vient alors v=w^pz=w^(p+q1).
				D'où le résultat pour q=p1+q1.
*)

(*
Exo 14 :

On pose A' = A\{a} et A'' = A\{a,b}

Le langage L recherché peut s'écrire :

	L = L2 ab L1

où L1 est le langage des mots qui ne contient pas le facteur aa
et L2 est le langage des mots de L1 qui ne se terminent pas par a.

On a L2 : (aA'+A') * => (un mot de L2 qui contient la lettre a est suivi d'une lette de A')
et L1 = L2(a+eps) => (correspond à un mot de L2 qui peut se terminer par la lettre a)

Ainsi, on a : L = (aA'+A') * ab(aA'+A') * (a+eps)
*)

(*
Exo 17 :

1.
*)

type arbre_expr = 
	Vide
	|Feuille of char
	|Etoile of arbre_expr
	|Concat of arbre_expr * arbre_expr
	|Plus of arbre_expr * arbre_expr;;

let rec arbre_to_chaine arbre = match arbre with
	Vide -> "0"
	|Feuille a -> String.make 1 a
	|Etoile t -> (arbre_to_chaine t)^"*"
	|Concat (g,d) -> "("^(arbre_to_chaine g)^"."^(arbre_to_chaine d)^")"
	|Plus (g,d) -> "("^(arbre_to_chaine g)^"+"^(arbre_to_chaine d)^")";;

(*
2.
*)

let chaine_to_arbre c =
	let p_arb = Stack.create () and p_sym = Stack.create () in
		for i=0 to (String.length c - 1) do
			if c.[i] = '0'
				then Stack.push Vide p_arb
			else if c.[i] = '*'
				then Stack.push (Etoile (Stack.pop p_arb)) p_arb
			else if c.[i] = '+' || c.[i] = '.'
				then Stack.push c.[i] p_sym
			else if c.[i]='('
				then ()
			else if c.[i]=')'
				then begin
					let b = Stack.pop p_arb in
					let a = Stack.pop p_arb in
					let o = Stack.pop p_sym in
						if o='+'
							then Stack.push (Plus (a,b)) p_arb
						else
							Stack.push (Concat (a,b)) p_arb
				end
			else Stack.push (Feuille c.[i]) p_arb
		done;
	Stack.pop p_arb;;

(*
3. Les expressions ne contenant pas le symbole eps : Une expression qui ne contiendrait pas le symbole * ne saurait contenir eps

	i) Dans un cas de base (une lettre), on renvoie false
	ii) Si e=(f)*, on renvoie true
	iii) Si e=e1+e2, on teste le langage e1 ou le langage e2
	iv) Si e=e1.e2, on teste si eps appartient à e1 et eps appartient à e2

	On a alors :
*)

let rec contient_epsilon arbre = match arbre with
	Vide -> false
	|Feuille a -> false
	|Etoile e -> true
	|Plus (e,f) -> (contient_epsilon e) || (contient_epsilon f)
	|Concat (e,f) -> (contient_epsilon e) && (contient_epsilon f);;

