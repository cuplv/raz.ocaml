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
 	val replace : dir -> 'a -> 'a cursor 'a cursor
 	val remove : dir -> 'a cursor -> 'a cursor
end

(* generate random levels for some ZIPPER implementations *)
let rnd_lev () = Bits.ffs0 (Random.bits())

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
		fun i zip -> 
		let with_index = List.mapi (fun i a -> (i,a)) in
    let without_index = List.map (fun (_,a) -> a) in
    let partition_indexed = List.partition (fun (i,_) -> i < index) in
    let (l,r) = partition_indexed (with_index zip) in
    (without_index l, without_index r)

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "move past front of sequence"
 		| R, _, [] -> failwith "move past end of sequence"
 		| L, e::l, r -> (l,e::r)
 		| L, l, e::r -> (e::l,r)

 	let view : dir -> 'a cursor -> 'a =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "view past front of sequence"
 		| R, _, [] -> failwith "view past end of sequence"
 		| L, e::_, _ -> e
 		| R, _, e::_ -> e

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> (e::l, r)
 		| R -> (l, e::r)

 	let replace : dir -> 'a -> 'a cursor 'a cursor =
 		fun d e (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "replace past front of sequence"
 		| R, _, [] -> failwith "replace past end of sequence"
 		| L, _::l, r -> (e::l,r)
 		| R, l, _::r -> (l, e::r)

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "remove past front of sequence"
 		| R, _, [] -> failwith "remove past end of sequence"
 		| L, _::l, r -> (l,r)
 		| R, l, _::r -> (l,r)

end

(* fingertree from ocaml batteries included *)
module FingerTree : ZIPPER = struct
	type 'a ftree = ('a, 'm) F.fg
	type 'a zipper = ftree
	type 'a cursor = ftree * ftree

  let empty : unit -> 'a cursor =
  	fun e -> (F.empty,F.empty)

  let singleton : 'a -> 'a cursor =
  	fun e -> (F.empty,F.singleton e)

 	let size : 'a zipper -> int =
 		fun zip -> F.size zip

 	let list_of_zipper : 'a zipper -> 'a list =
 		fun zip -> F.to_list zip

 	let lists_of_cursor : 'a cursor -> 'a list * 'a list =
 		fun (l,r) -> (F.to_list l, F.to_list r)

 	let unfocus : 'a cursor -> 'a zipper =
 		fun (l,r) -> F.append l r

	let focus : int -> 'a zipper -> 'a cursor =
		fun i zip -> F.split_at zip i

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d with
 		| L -> begin
 			match F.rear l with
 			| None -> failwith "move past front of sequence"
 			| Some(l, e) -> (l, F.cons r e)
 		end
 		| R -> begin
 			match F.front r with
 			| None -> failwith "move past end of sequence"
 			| Some(r, e) -> (F.snoc l e, r)
		end

 	let view : dir -> 'a cursor -> 'a =
 		fun d (l,r) -> match d with
 		| L -> begin
 			match F.last l with
 			| None -> failwith "view past front of sequence"
 			| Some(e) -> e
 		end
 		| R -> begin
 			match F.head r with
 			| None -> failwith "view past end of sequence"
 			| Some(e) -> e
		end

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> (F.snoc l e, r)
 		| R -> (l, F.cons r e)

 	let replace : dir -> 'a -> 'a cursor 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> begin
 			match F.rear l with
 			| None -> failwith "replace past front of sequence"
 			| Some(l, _) -> (F.snoc l e, r)
 		end
 		| R -> begin
 			match F.front r with
 			| None -> failwith "replace past end of sequence"
 			| Some(r, _) -> (l, F.cons r e)
		end

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d with
 		| L -> begin
 			match F.rear l with
 			| None -> failwith "remove past front of sequence"
 			| Some(l, _) -> (l, r)
 		end
 		| R -> begin
 			match F.front r with
 			| None -> failwith "remove past end of sequence"
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
  	fun e -> Raz.singleton e

 	let size : 'a zipper -> int =
 		fun zip -> Raz.item_count zip

 	let list_of_zipper : 'a zipper -> 'a list =
	  let rec drain_l r l size =
	    match size with
	    | 0 -> l
	    | _ ->
	    let l = (Raz.view Raz.L r)::l in
	    let r = Raz.remove Raz.L r in
	    drain r l (size - 1)
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
		fun i zip -> (R, Raz.focus i zip)

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (cd,raz) -> match d, cd with
 		| L, L -> (R,raz)
 		| L, R -> try (R,Raz.move Raz.L raz)
 			with _ -> "move past front of sequence"
 		| R, L -> try (L,Raz.move Raz.R raz)
 			with _ -> "move past end of sequence"
 		| R, R -> (L,raz)

 	let view : dir -> 'a cursor -> 'a =
 		fun d (cd,raz) -> match d, cd with
 		| L, L -> Raz.view_c raz
 		| L, R -> try Raz.view_c (Raz.move Raz.L raz)
 			with _ -> "view past front of sequence"
 		| R, L -> try Raz.view_c (Raz.move Raz.R raz)
 			with _ -> "view past end of sequence"
 		| R, R -> Raz.view_c raz

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (cd,raz) -> match d, cd with
 		| L, L -> (cd,Raz.move Raz.R (Raz.insert Raz.R e raz))
 		| L, R -> (cd,Raz.insert Raz.L e raz)
 		| R, L -> (cd,Raz.insert Raz.R e raz)
 		| R, R -> (cd,Raz.move Raz.L (Raz.insert Raz.L e raz))

 	let replace : dir -> 'a -> 'a cursor 'a cursor =
 		fun d e (cd,r) -> match d, cd with
 		| L, L -> (L,Raz.alter_c e raz)
 		| L, R -> try (L,Raz.alter_c e (Raz.move Raz.L raz))
 			with _ -> failwith "replace past front of sequence"
 		| R, L -> try (R,Raz.alter_c e (Raz.move Raz.R raz))
 			with _ -> failwith "replace past end of sequence"
 		| R, R -> (R,Raz.alter_c e raz)

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (cd,raz) -> match d, cd with
 		| L, L -> try (L,Raz.remove Raz.R (Raz.move Raz.L raz))
 			with _ -> try (R,Raz.remove Raz.L (Raz.move Raz.R raz))
 			with _ -> failwith "RazSimple cannot remove last item"
 		| L, R -> try (R,Raz.remove Raz.L raz)
 			with _ -> failwith "remove past front of sequence"
 		| R, L -> try (L,Raz.remove Raz.R raz)
 			with _ -> failwith "remove past end of sequence"
 		| R, R -> try (R,Raz.remove Raz.L (Raz.move Raz.R raz))
 			with _ -> try (L,Raz.remove Raz.R (Raz.move Raz.L raz))
 			with _ -> failwith "RazSimple cannot remove last item"

end

(* more structured implementation of RAZ *)
module RazFormal : ZIPPER = struct
	type 'a zipper = 'a list
	type 'a cursor = 'a list * 'a list

  let empty : unit -> 'a cursor =
  	fun () -> ([],[])

  let singleton : 'a -> 'a cursor =
  	fun () -> ([],[])

 	let size : 'a zipper -> int =
 		fun zip -> List.length zip

 	let list_of_zipper : 'a zipper -> 'a list =
 		fun zip -> zip

 	let lists_of_cursor : 'a cursor -> 'a list * 'a list =
 		fun (l,r) -> (List.rev l,r)

 	let unfocus : 'a cursor -> 'a zipper =
 		fun (l,r) -> (List.rev l) @ r

	let focus : int -> 'a zipper -> 'a cursor =
		fun i zip -> 
		let with_index = List.mapi (fun i a -> (i,a)) in
    let without_index = List.map (fun (_,a) -> a) in
    let partition_indexed = List.partition (fun (i,_) -> i < index) in
    let (l,r) = partition_indexed (with_index zip) in
    (without_index l, without_index r)

 	let move : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "move past front of sequence"
 		| R, _, [] -> failwith "move past end of sequence"
 		| L, e::l, r -> (l,e::r)
 		| L, l, e::r -> (e::l,r)

 	let view : dir -> 'a cursor -> 'a =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "view past front of sequence"
 		| R, _, [] -> failwith "view past end of sequence"
 		| L, e::_, _ -> e
 		| R, _, e::_ -> e

 	let insert : dir -> 'a -> 'a cursor -> 'a cursor =
 		fun d e (l,r) -> match d with
 		| L -> (e::l, r)
 		| R -> (l, e::r)

 	let replace : dir -> 'a -> 'a cursor 'a cursor =
 		fun d e (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "replace past front of sequence"
 		| R, _, [] -> failwith "replace past end of sequence"
 		| L, _::l, r -> (e::l,r)
 		| R, l, _::r -> (l, e::r)

 	let remove : dir -> 'a cursor -> 'a cursor =
 		fun d (l,r) -> match d, l, r with
 		| L, [], _ -> failwith "remove past front of sequence"
 		| R, _, [] -> failwith "remove past end of sequence"
 		| L, _::l, r -> (l,r)
 		| R, l, _::r -> (l,r)

end
