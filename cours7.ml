(* ========================================== *)
let calcul a = let c = ref (-1) in 
	let rec aux b h = match b with
		Nil -> (0,h)
			|N(g,_,d) -> let (ng,pg) = aux g!

(* À terminer *)
(* ========================================== *)

let calcul a = 
	let rec aux b h = 
		match b with
			Nil -> 0
				|N(g,_,d) -> (aux g (h+1))+(aux d (h+1))+h;;