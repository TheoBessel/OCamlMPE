let tab = [|[|0;2;1|];[|1;0;2|]|]

let teste m = 
	let e = ref 0 in
	for i = 0 to String.length m-1 do
		let l = if m.[i] = '0' then 0 else 1 in
		e := tab.(l).(!e) done;
	!e = 0;;