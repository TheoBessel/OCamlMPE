let rec map f l = match l with
	[] -> []
		|h::t -> (f h)::(map f t);;

let rec partie l = match l with
	[] -> [[]]
		|h::t -> let p = partie t in map (function l -> h::l) p @ p;;

let a = partie [1;2];;

(*let largeur a = 
	let rec aux b queue*)

let rec defile l = match l with
	[] -> failwith "erreur"
		|[x] -> x,[]
			|h::t -> let d = defile t in fst(d),h::snd(d);;

let rec largeur a q = match q with
	[] -> (); largeur a (a::q) 
		|f -> let N(g,e,d),m = defile f in print_char e; largeur a (d::(g::m));;

let inv = reverse [1;2;3;4];;

let largeur a = 
	let rec etape enf def = match (enf,def) with
		[],[] -> ()
			|_,Nil::r -> etape enf r
				|_,N(g,x,d)::r -> print_char x; etape (d::g::enf) r
					|_,[] -> etape [] (List.rev enf)
	in etape [] [a];;

type 'a arbregen = M of 'a * (('a arbregen) list);;

let a = M('a',[M('b',[]);M('c',[]);M('d',[]);M('e',[M('f',[]);M('g',[])])]);;

let rec hauteur (M(x,l)) = match l with
	[] -> 0
		|h::t -> max (1+hauteur h) (hauteur (M(x,t)));;

let rec taille (M(x,l)) = match l with
	[] -> 1
		|h::t -> taille h + taille (M(x,t));;

let rec feuilles (M(x,l)) = match l with
	[] -> 1
		|[a] -> feuilles a
			|h::t -> feuilles h + feuilles (M(x,t));;

let a = M('1',[M('2',[M('5',[]);M('6',[])]);M('3',[]);M('4',[M('7',[])])]);;
let a = M(1,[M(2,[M(5,[]);M(6,[])]);M(3,[]);M(4,[M(7,[])])]);;

let rec gentobin (M(x,l)) = match l with
	[] -> N(Nil,x,Nil)
		|h::t -> let N(g,e,Nil) = gentobin h
				in let N(d,_,Nil) = gentobin (M(x,t))
					in N(N(g,e,d),x,Nil);;

let rec bintogen (N(a,x,Nil)) = match a with
	Nil -> M(x,[])
		|N(g,e,d) -> let t = bintogen (N(g,e,Nil)) in 
			let M(y,l) = bintogen (N(d,x,Nil)) in 
				M(y,t::l);;