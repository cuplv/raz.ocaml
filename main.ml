module F = Fingertree
module Raz = Raz_simp

let print_raz = Raz.print_raz (fun e -> e)

let rec print_ft ft =
	match F.head ft with
	| None -> print_endline ""
	| Some(h) -> 
		print_string h;
		print_string "; ";
		print_ft (F.tail_exn ft)

let time thunk =
	let start = Unix.gettimeofday () in
	let res = thunk() in
    let stop = Unix.gettimeofday () in
    let t = (stop -. start) in
    (t,res)

let test () =
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
(*--------*)
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

	print_ft f;
	None

let eval () =
	let rec insert_ft n ft =
		if n <= 0 then ft else
		let ft = F.snoc ft n in
		insert_ft (n-1) ft
	in
	let rec insert_r n r =
		if n <= 0 then r else
		let r = Raz.insert Raz.L n r in
		insert_r (n-1) r
	in
	let rec rnd_insert_ft sz n ft =
		if n <= 0 then ft else
		let p = Random.int (sz+1) in
  		let left, right = F.split_at ft p in
  		let ft = F.append (F.snoc left n) right in
		rnd_insert_ft (sz+1) (n-1) ft
	in
	let rec rnd_insert_r sz n r =
		if n <= 0 then r else
		let p = Random.int (sz+1) in
		let r = Raz.focus (Raz.unfocus r) p in
		let r = Raz.insert Raz.L n r in
		rnd_insert_r (sz+1) (n-1) r
	in

	let r = Raz.singleton 0 |> Raz.insert Raz.L 0 in
	let ft = F.snoc (F.singleton 0) 0 in
	Printf.printf "Test,Param,RAZ,Fingertree\n%!";
	for i = 0 to 100 do
		let n = 1000000 in
		let (t_r,_) = time (fun()->insert_r n r) in
		let (t_ft,_) = time (fun()->insert_ft n ft) in
		Printf.printf "Insert,%d,%.4f,%.4f\n%!" n t_r t_ft;
	done;
	for i = 0 to 100 do
		let n = 100000 in
		let seed = Random.bits() in
		Random.init seed;
		let (t_r,_) = time (fun()->rnd_insert_r 0 n r) in
		Random.init seed;
		let (t_ft,_) = time (fun()->rnd_insert_ft 0 n ft) in
		Printf.printf "RndSingle,%d,%.4f,%.4f\n%!" n t_r t_ft;
	done;

	None


(* let _ = test() *)
let _ = eval()
