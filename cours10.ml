let rec decoupe l = match l with
	[] -> [],[]
	|[x] -> [x],[]
	|h1::h2::t -> let (t1,t2) = (decoupe t) 
		in (h1::t1,h2::t2);;

let rec fusion l1 l2 = match (l1,l2) with
	[], l2 -> l2
	|l1, [] -> l1
	|h1::t1, h2::t2 when h1 <= h2 -> h1::(fusion t1 (h2::t2))
	|h1::t1, h2::t2 -> h2::(fusion (h1::t1) t2);;

let rec tri_fusion l = match l with
	[] -> []
	|[x] -> [x]
	|l ->  let (l1,l2) = (decoupe l)
		in (fusion (tri_fusion l1) (tri_fusion l2));;

"on lit de la gauche vers la droite, vous pourrez vérifier ce soir"