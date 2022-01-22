let rec bezout a b = match b with
	0 -> (1,0)
		|b -> let u,v = bezout b (a mod b)
			in (v,u-v*(a/b));;

let rec croissante l = match l with
	[] -> true
		|[a] -> true
			|a::b::q -> a<=b && croissante (b::q);;

let infixe a = 
	let rec aux b acc = match b with
		Nil -> acc
			|N(g,x,d) -> let l = aux d acc in
				aux g (x::l) in
			aux a [];;

let test a = croissante (infixe a);;

let abr1 = N(N(N(Nil,2,Nil),4,N(Nil,5,Nil)),6,N(N(Nil,7,Nil),8,Nil));;
let abr2 = N(N(N(Nil,4,Nil),5,N(Nil,9,Nil)),13,N(N(Nil,18,Nil),23,Nil));;

(*
Exo 11 : On va utiliser le principe diviser pour régner pour parcourir le tableau.

	On prend le terme médian du tableau t(m).

	On construit les fils gauche g et droite d à partir des moitiés t(0)...t(m-1) et t(m+1)...t(n)
	et on renvoie N(g,t(m),d)
*)

let efficace t =
	let rec aux i j =
		if i > j then Nil
			else let m = (i+j)/2 in
				N(aux i (m-1),t.(m),aux (m+1) j) in
		aux 0 ((Array.length t)-1);;

(*
Analyse de la complexité :
	
	Notons C(n) la complexité de aux lorsque j-i+1=n. On a alors :

		C(n) = O(1)+C([n/2]+1)+C([n/2])

	On peut raisonnablement supposer n -> c(n) croissante

	Introduisons T(p) = C(2^p). On a alors T(p) = O(1)+2T(p). ie T(p)/(2^p) = O(1/(2^p))+T(p)/(2^(p-1))

	T(k)/(2^k) = O(Somme(r=0->k)de(1/(2^r))) = O(1). ie T(p) = O(2^p)

	Soit p tel que 2^p <= n < 2^(p+1). On a : C(2^p) <= C(n) < C(2^(p+1)).

	T(p) <= C(n) <= T(p+1) soit C(n) = O(2^p) = O(n)

	Remarque : p = [ln_2(n)] correspond à la hauteur d'un arbre binaire de recherche équilibré de taille n.
*)

let rec recherche t x = match t with
	Nil -> false
		|N(tg,e,td) -> if x=e then true
			else if x>e then recherche td x
				else recherche tg x;;

let rec insere t x = match t with
	Nil -> N(Nil,x,Nil)
		|N(tg,e,td) -> if x=e then t
			else if x>e then N(tg,e,insere td x)
				else N(insere tg x,e,td);;

let rec supp_max t = match t with
	N(tg,x,Nil) -> (x,tg)
		|N(tg,x,td) -> let (e,dr) = supp_max td in
			(e,N(tg,x,dr));;

let rec supprime t x = match t with
	Nil -> Nil
		|N(tg,e,td) -> if x<e then N(supprime tg x,e,td)
			else if tg=Nil then td
				else let (m,tgr)=supp_max tg
					in N(tgr,m,td);;

let rec decoupe a x = match a with
	Nil -> (Nil,Nil)
		|N(ga,ra,da) -> if x<ra then let u,v = decoupe ga x
			in (u,N(v,ra,da))
				else if ra<x then let u,v = decoupe da x
					in (N(ga,ra,u),v)
						else (ga,da);; (* x=ra *)

let rec fusion a b = match a with
	Nil -> b
		|N(g,r,d) -> let u,v = decoupe b r
			in N(fusion u g,r,fusion v d);;
	