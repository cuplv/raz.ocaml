(* 
This file creates a common interface amoung zipper-based structures

RAZ code is all in  'raz_simp.ml', and 'raz_2.ml'

Fingertree impl is in 'fingertree.ml'
'bat*.ml' files support the fingertree code.
Fingertree and supporting files are from 'https://github.com/ocaml-batteries-team/batteries-included'
*)

module F = Fingertree
module Raz = Raz_simp
module Raz2 = Raz_2.Raz

(* cursors are between elements, so some functions need to specify the applicable direction *)
type dir = L | R
  
(* common interface to all zipper-based data structures *)
module type ZIPPER = sig
	(* zipper as a whole - limited functionality *)
  type 'a zipper
  (* zipper with cursor - main functionality *)
  type 'a cursor

  (* create a cursor with no content *)
  val empty : unit -> 'a cursor
  (* create first value, with cursor after it *)
  val singleton : 'a -> 'a cursor
  (* the number of items in the zipper *)
 	val size : 'a zipper -> int

 	(* conversions to native lists, left to right *)
 	val list_of_zipper : 'a zipper -> 'a list
 	val lists_of_cursor : 'a cursor -> 'a list * 'a list

 	(* remove cursor to allow some functionality *)
 	val unfocus : 'a cursor -> 'a zipper
 	(* adds a cursor before the indicated element *)
	val focus : int -> 'a zipper -> 'a cursor
 	
 	(* common cursor operations, from a cursor between elements *)
 	val move : dir -> 'a cursor -> 'a cursor
 	val view : dir -> 'a cursor -> 'a
 	val insert : dir -> 'a -> 'a cursor -> 'a cursor
 	val replace : dir -> 'a -> 'a cursor -> 'a cursor
 	val remove : dir -> 'a cursor -> 'a cursor
end

(* generate random levels for some ZIPPER implementations *)
let rnd_level () = Bits.ffs0 (Random.bits())

(* inefficent but obviously correct implementation *)
module Seq : ZIPPER = struct
	type 'a zipper = 'a list
	type 'a cursor = 'a list * 'a list

  let empty : unit -> 'a cursor =
  	fun e -> ([],[])

  let singleton : 'a -> 'a cursor =
  	fun e -> ([e],[])

 	let size : 'a zipper -> int =
 		fun zip -> List.length zip

 	let list_of_zipper : 'a zipper -> 'a list =
 		fun zip -> zip

 	let lists_of_cursor : 'a cursor -> 'a list * 'a list =
 		fun (l,r) -> (List.rev l,r)

 	let unfocus : 'a cursor -> 'a zipper =
 		fun (l,r) -> (List.rev l) @ r

	let focus : int -> 'a zipper -> 'a cursor =
		fun index zip ->
		let with_index = List.mapi (fun i a -> (i,a)) in
    let without_index = List.map (fun (_,a) -> a) in
    let partition_indexed = List.partition (fun (i,_) -> i < index) in
    let (l,r) = partition_indexed (with_index zip) in
    (without_index (List.rev l), without_index r)

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "Seq: move past front of sequence"
 		| R, _, [] -> failwith "Seq: move past end of sequence"
 		| L, e::l, r -> (l,e::r)
 		| R, l, e::r -> (e::l,r)

 	let view : dir -> 'a cursor -> 'a =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "Seq: view past front of sequence"
 		| R, _, [] -> failwith "Seq: view past end of sequence"
 		| L, e::_, _ -> e
 		| R, _, e::_ -> e

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> (e::l, r)
 		| R -> (l, e::r)

 	let replace : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "Seq: replace past front of sequence"
 		| R, _, [] -> failwith "Seq: replace past end of sequence"
 		| L, _::l, r -> (e::l,r)
 		| R, l, _::r -> (l, e::r)

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "Seq: remove past front of sequence"
 		| R, _, [] -> failwith "Seq: remove past end of sequence"
 		| L, _::l, r -> (l,r)
 		| R, l, _::r -> (l,r)

end

(* fingertree from ocaml batteries included *)
module FingerTree : ZIPPER = struct
	(*
		Fingertrees naturally have edit points on each end,
		so we do not reverse the left side of the zipper,
		instead we use the appropriate function for the
		direction
	*)
	type 'a ftree = 'a F.t
	type 'a zipper = 'a ftree
	type 'a cursor = 'a ftree * 'a ftree

  let empty : unit -> 'a cursor =
  	fun e -> (F.empty,F.empty)

  let singleton : 'a -> 'a cursor =
  	fun e -> (F.empty,F.singleton e)

 	let size : 'a zipper -> int =
 		fun zip -> F.size zip

 	let list_of_zipper : 'a zipper -> 'a list =
 		fun zip ->
 		let rec drain_l ft l =
	    match F.last ft with
	    | None -> l
	    | Some(e) ->
	    let l = e::l in
	    match F.init ft with
	    | None -> l
	    | Some(ft) ->
	    drain_l ft l
	  in
	  drain_l zip []


 	let lists_of_cursor : 'a cursor -> 'a list * 'a list =
 		fun (l,r) -> (list_of_zipper l, list_of_zipper r)

 	let unfocus : 'a cursor -> 'a zipper =
 		fun (l,r) -> F.append l r

	let focus : int -> 'a zipper -> 'a cursor =
		fun index zip -> F.split_at zip index

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d with
 		| L -> begin
 			match F.rear l with
 			| None -> failwith "FingerTree: move past front of sequence"
 			| Some(l, e) -> (l, F.cons r e)
 		end
 		| R -> begin
 			match F.front r with
 			| None -> failwith "FingerTree: move past end of sequence"
 			| Some(r, e) -> (F.snoc l e, r)
		end

 	let view : dir -> 'a cursor -> 'a =
 		fun d (l,r) -> match d with
 		| L -> begin
 			match F.last l with
 			| None -> failwith "FingerTree: view past front of sequence"
 			| Some(e) -> e
 		end
 		| R -> begin
 			match F.head r with
 			| None -> failwith "FingerTree: view past end of sequence"
 			| Some(e) -> e
		end

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> (F.snoc l e, r)
 		| R -> (l, F.cons r e)

 	let replace : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> begin
 			match F.rear l with
 			| None -> failwith "FingerTree: replace past front of sequence"
 			| Some(l, _) -> (F.snoc l e, r)
 		end
 		| R -> begin
 			match F.front r with
 			| None -> failwith "FingerTree: replace past end of sequence"
 			| Some(r, _) -> (l, F.cons r e)
		end

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d with
 		| L -> begin
 			match F.rear l with
 			| None -> failwith "FingerTree: remove past front of sequence"
 			| Some(l, _) -> (l, r)
 		end
 		| R -> begin
 			match F.front r with
 			| None -> failwith "FingerTree: remove past end of sequence"
 			| Some(r, _) -> (l, r)
		end

end

(* simplistic implementation of RAZ *)
(* this version cannot be empty, or it will throw an exception *)
module RazSimple : ZIPPER = struct
	(*
	Here we have to deal with the fact that this
	raz is focused on an element, and can't focus
	on the (non-element) far ends of the sequence.

	We include meta data about where the
	implementation cursor is in relation to the
	api cursor, and use this to select where to
	operate. Additional movements are required,
	but they are efficient because none will require
	an additional trim (except for a special case
	in remove).

	the implementation cursor is on the
	(dir in cursor) side of this api cursor
	*)
	type 'a zipper = 'a Raz.tree
	type 'a cursor = dir * 'a Raz.raz

  let empty : unit -> 'a cursor =
  	fun e -> failwith "RazSimple cannot be empty"

  let singleton : 'a -> 'a cursor =
  	fun e -> (R,Raz.singleton e)

 	let size : 'a zipper -> int =
 		fun zip -> Raz.item_count zip

 	let list_of_zipper : 'a zipper -> 'a list =
	  let rec drain_l r l size =
	    match size with
	    | 0 -> l
	    | _ ->
	    let l = (Raz.view Raz.L r)::l in
	    let r = Raz.remove Raz.L r in
	    drain_l r l (size - 1)
	  in
	  fun zip ->
	  let size = Raz.item_count zip in
	  let raz = Raz.focus zip size in
	  drain_l raz [Raz.view_c raz] (size - 1)

 	let lists_of_cursor : 'a cursor -> 'a list * 'a list =
 		let rec drain d r l =
 			try 
 				let l = (Raz.view d r)::l in
 				drain d (Raz.remove d r) l
 			with _ -> l
 		in
 		fun (cd,raz) ->
 		match cd with
 		| L -> (drain Raz.L raz [Raz.view_c raz], List.rev (drain Raz.R raz []))
 		| R -> (drain Raz.L raz [], List.rev (drain Raz.R raz [Raz.view_c raz]))

 	let unfocus : 'a cursor -> 'a zipper =
 		fun (d,raz) -> Raz.unfocus raz

	let focus : int -> 'a zipper -> 'a cursor =
		fun index zip -> (R, Raz.focus zip index)

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (cd,raz) -> match d, cd with
 		| L, L -> (R,raz)
 		| L, R -> begin try (R,Raz.move Raz.L raz)
 			with _ -> failwith "RazSimple: move past front of sequence"
 		end
 		| R, L -> begin try (L,Raz.move Raz.R raz)
 			with _ -> failwith "RazSimple: move past end of sequence"
 		end
 		| R, R -> (L,raz)

 	let view : dir -> 'a cursor -> 'a =
 		fun d (cd,raz) -> match d, cd with
 		| L, L -> Raz.view_c raz
 		| L, R -> begin try Raz.view_c (Raz.move Raz.L raz)
 			with _ -> failwith "RazSimple: view past front of sequence"
 		end
 		| R, L -> begin try Raz.view_c (Raz.move Raz.R raz)
 			with _ -> failwith "RazSimple: view past end of sequence"
 		end
 		| R, R -> Raz.view_c raz

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (cd,raz) -> match d, cd with
 		| L, L -> (cd,Raz.move Raz.R (Raz.insert Raz.R e raz))
 		| L, R -> (cd,Raz.insert Raz.L e raz)
 		| R, L -> (cd,Raz.insert Raz.R e raz)
 		| R, R -> (cd,Raz.move Raz.L (Raz.insert Raz.L e raz))

 	let replace : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (cd,raz) -> match d, cd with
 		| L, L -> (L,Raz.alter_c e raz)
 		| L, R -> begin try (L,Raz.alter_c e (Raz.move Raz.L raz))
 			with _ -> failwith "RazSimple: replace past front of sequence"
 		end
 		| R, L -> begin try (R,Raz.alter_c e (Raz.move Raz.R raz))
 			with _ -> failwith "RazSimple: replace past end of sequence"
 		end
 		| R, R -> (R,Raz.alter_c e raz)

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (cd,raz) -> match d, cd with
 		| L, L -> begin 
 		try (L,Raz.remove Raz.R (Raz.move Raz.L raz))
 			with _ -> begin
 				try (R,Raz.remove Raz.L (Raz.move Raz.R raz))
 				with _ -> failwith "RazSimple cannot remove last item"
	 		end
	 	end
 		| L, R -> begin try (R,Raz.remove Raz.L raz)
 			with _ -> failwith "RazSimple: remove past front of sequence"
 		end
 		| R, L -> begin try (L,Raz.remove Raz.R raz)
 			with _ -> failwith "RazSimple: remove past end of sequence"
 		end
 		| R, R -> begin
 		try (R,Raz.remove Raz.L (Raz.move Raz.R raz))
 			with _ -> begin
 				try (L,Raz.remove Raz.R (Raz.move Raz.L raz))
 				with _ -> failwith "RazSimple cannot remove last item"
 			end
 		end

end

(* more structured implementation of RAZ *)
module RazFormal : ZIPPER = struct
	(* 
		This implementation will not provide errors when
		operating past the end of a sequence. Instead, it
		will return the sequence unedited. We perform
		pointer equality to check for the error.
	*)
	type 'a zipper = 'a Raz2.tree
	type 'a cursor = 'a Raz2.zip

  let empty : unit -> 'a cursor =
  	fun () -> Raz2.empty (rnd_level())

  let singleton : 'a -> 'a cursor =
  	fun e -> Raz2.insert Raz2.L e (rnd_level()) (Raz2.empty (rnd_level()))

 	let size : 'a zipper -> int =
 		fun zip -> Raz2.elm_cnt zip

 	let list_of_zipper : 'a zipper -> 'a list =
	  let rec drain_l raz2 l =
	    match Raz2.peek Raz2.L raz2 with
	    | None -> l
	    | Some(e) ->
	    let l = e::l in
	    let raz2 = Raz2.do_cmd (Raz2.Remove(Raz2.L)) raz2 in
	    drain_l raz2 l
	  in
 		fun zip ->
	  let size = Raz2.elm_cnt zip in
	  let raz2 = Raz2.focus zip size in
	  drain_l raz2 []

 	let lists_of_cursor : 'a cursor -> 'a list * 'a list =
 		let rec drain d raz2 l =
 			match Raz2.peek d raz2 with
 			| None -> l
 			| Some(e) ->
 				let l = e::l in
 				drain d (Raz2.do_cmd (Raz2.Remove(d)) raz2) l
 		in
 		fun raz2 ->
 		(drain Raz2.L raz2 [], List.rev (drain Raz2.R raz2 []))

 	let unfocus : 'a cursor -> 'a zipper =
 		fun raz2 -> Raz2.unfocus raz2

	let focus : int -> 'a zipper -> 'a cursor =
		fun index zip -> Raz2.focus zip index

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d raz2 -> match d with
 		| L ->
 			let out = Raz2.do_cmd (Raz2.Move(Raz2.L)) raz2 in
 			if out == raz2 then failwith "RazFormal: move past front of sequence"
 			else out
 		| R -> 
 			let out = Raz2.do_cmd (Raz2.Move(Raz2.R)) raz2 in
 			if out == raz2 then failwith "RazFormal: move past end of sequence"
 			else out

 	let view : dir -> 'a cursor -> 'a =
 		fun d raz2 -> match d with
 		| L -> begin
 			match Raz2.peek Raz2.L raz2 with
 			| None -> failwith "RazFormal: view past front of sequence"
 			| Some(e) -> e
 		end 
 		| R -> begin
 			match Raz2.peek Raz2.R raz2 with
 			| None -> failwith "RazFormal: view past end of sequence"
 			| Some(e) -> e
 		end 

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e raz2 -> match d with
 		| L -> Raz2.insert Raz2.L e (rnd_level()) raz2
 		| R -> Raz2.insert Raz2.R e (rnd_level()) raz2

 	let replace : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e raz2 -> match d with
 		| L ->
 			let out = Raz2.do_cmd (Raz2.Replace(Raz2.L,e)) raz2 in
 			if out == raz2 then failwith "RazFormal: replace past front of sequence"
 			else out
 		| R -> 
 			let out = Raz2.do_cmd (Raz2.Replace(Raz2.R,e)) raz2 in
 			if out == raz2 then failwith "RazFormal: replace past end of sequence"
 			else out

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d raz2 -> match d with
 		| L ->
 			let out = Raz2.do_cmd (Raz2.Remove(Raz2.L)) raz2 in
 			if out == raz2 then failwith "RazFormal: remove past front of sequence"
 			else out
 		| R -> 
 			let out = Raz2.do_cmd (Raz2.Remove(Raz2.R)) raz2 in
 			if out == raz2 then failwith "RazFormal: remove past end of sequence"
 			else out

end
