module F = Fingertree
module Raz = Raz_simp

let print_raz = Raz.print_raz (fun e -> e)

let _ =
	Random.self_init();
	let _ = print_int (Raz.rnd_level()); print_endline "" in
	let r = Raz.singleton "r" in
	let r = Raz.insert Raz.L "e" r in
	let r = Raz.insert Raz.L "f" r in
	let r = Raz.insert Raz.L "g" r in
	let r = Raz.insert Raz.L "h" r in
	let r = Raz.insert Raz.L "i" r in
	let r = Raz.insert Raz.L "j" r in
	let r = Raz.insert Raz.L "k" r in
	let r = Raz.insert Raz.L "l" r in
	let r = Raz.insert Raz.L "m" r in
	let r = Raz.insert Raz.L "n" r in
	let r = Raz.insert Raz.L "o" r in
	let r = Raz.insert Raz.L "p" r in
	let r = Raz.insert Raz.L "q" r in
	let r = Raz.focus (Raz.unfocus r) 4 in
	let r = Raz.move Raz.L r in
	let e = Raz.view_c r in
	let _ = print_endline e in
	let r = Raz.focus (Raz.unfocus r) 7 in
	let e = Raz.view_c r in
	let _ = print_endline e in
	let r = Raz.alter_c "L" r in
	let r = Raz.focus (Raz.unfocus r) 7 in
	let _ = print_raz r in
	let e = Raz.view_c r in
	let _ = print_endline e in
(*--------*)
	let rec lots_of_refocus r =
		print_string ".";
		let r = Raz.focus (Raz.unfocus r) (Random.int 14) in
		if (Random.int 100) < 1 then (print_endline ""; r) else
		lots_of_refocus r
	in
	let r = lots_of_refocus r in
(*--------*)
	let push_l r = Raz.insert Raz.L r in
	let push_r r = Raz.insert Raz.R r in
	let pop_l r = (Raz.view Raz.L r,Raz.remove Raz.L r) in
	let pop_r r = (Raz.view Raz.R r,Raz.remove Raz.R r) in
	let r = Raz.empty "" in
	let r = push_l "1" r in
	let r = push_l "2" r in
	let r = push_l "3" r in
	let r = push_l "4" r in
	let r = push_l "5" r in
	let r = Raz.focus (Raz.unfocus r) 0 in
	(* let _ = print_raz r in *)
	let (a,r) = pop_r r in
	let _ = print_endline (a) in
	let (b,r) = pop_r r in
	let _ = print_endline (b) in
	let (c,r) = pop_r r in
	let _ = print_endline (c) in
	(* let _ = print_endline (a^b^c) in *)

	let f = F.singleton "a" in
	let f = F.snoc f "b" in
	let f = F.snoc f "c" in
	let f = F.snoc f "d" in
	let f = F.snoc f "e" in
	let f = F.snoc f "f" in
	let f = F.snoc f "g" in
	let f = F.snoc f "h" in
	let f = F.snoc f "i" in
	let f = F.set f 3 "D" in

	let rec print_ft ft =
		match F.head ft with
		| None -> print_endline ""
		| Some(h) -> 
			print_string h;
			print_string "; ";
			print_ft (F.tail_exn ft)
	in
	print_ft f;
	None

