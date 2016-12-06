(* 
This file is mostly tests of correctness.

RAZ code is all in  'raz_simp.ml', and 'raz_2.ml'

Fingertree impl is in 'fingertree.ml'
'bat*.ml' files support the fingertree code.
Fingertree and supporting files are from 'https://github.com/ocaml-batteries-team/batteries-included'
*)

module F = Fingertree
module Raz = Raz_simp
module Raz2 = Raz_2.Raz

(* inefficient but correct sequence implementation to use as baseline *)
module Seq = struct
  type seq = int list
  let singleton value = [value]
  let insert value index seq =
    let with_index = List.mapi (fun i a -> (i,a)) seq in
    let (left,right) = List.partition (fun (i,_) -> i < index) with_index in
    let with_val = left @ ((0,value)::right) in
    let without_index = List.map (fun (_,a) -> a) with_val in
    without_index
  let to_list seq = seq

end

module Params = struct
  let rnd_seed_ = ref 0         (* seed value for random number generation *)

  let args = [
    ("--seed",    Arg.Set_int rnd_seed_,  " random seed");
  ]

  let _ = Arg.parse args
    (fun arg -> invalid_arg ("Unknown: "^arg))
    "usage: eval [options]"

  let rnd_seed = !rnd_seed_
end

type elm = int

let rnd_level = Raz.rnd_level

let to_list_seq = Seq.to_list

let to_list_ft ft =
  let rec drain ft l =
    match F.last ft with
    | None -> l
    | Some(e) ->
    let l = e::l in
    match F.init ft with
    | None -> l
    | Some(ft) ->
    drain ft l
  in
  drain ft []

let to_list_r r =
  let t = Raz.unfocus r in
  let size = Raz.item_count t in
  let r = Raz.focus t size in
  let rec drain r l size =
    match size with
    | 0 -> l
    | _ ->
    let l = (Raz.view Raz.L r)::l in
    let r = Raz.remove Raz.L r in
    drain r l (size - 1)
  in
  drain r [(Raz.view_c r)] (size - 1)

let to_list_r2 r2 =
  let t = Raz2.unfocus r2 in
  let size = Raz2.elm_cnt t in
  let r2 = Raz2.focus t size in
  let rec drain r2 l =
    match Raz2.peek Raz2.L r2 with
    | None -> l
    | Some(e) ->
    let l = e::l in
    let r2 = Raz2.do_cmd (Raz2.Remove(Raz2.L)) r2 in
    drain r2 l
  in
  drain r2 []  

let rec insert_seq inserts seq =
  match inserts with
  | [] -> seq
  | (p,v)::other_inserts ->
  let seq = Seq.insert v p seq in
  insert_seq other_inserts seq

let rec insert_ft inserts ft =
  match inserts with
  | [] -> ft
  | (p,v)::other_inserts ->
  let left, right = F.split_at ft p in
  let ft = F.append (F.snoc left v) right in
  insert_ft other_inserts ft

let rec insert_r inserts r =
  match inserts with
  | [] -> r
  | (p,v)::other_inserts ->
  let t = Raz.unfocus r in
  let r = Raz.focus t p in
  let r = Raz.insert Raz.L v r in
  insert_r other_inserts r

let rec insert_r2 inserts r2 =
  match inserts with
  | [] -> r2
  | (p,v)::other_inserts ->
  let lev = rnd_level() in
  let t  = Raz2.unfocus r2 in
  let r2 = Raz2.focus t p in
  let r2 = Raz2.insert Raz2.L v lev r2 in
  insert_r2 other_inserts r2

let test() =
  (* init seqs *)
  let r = Raz.singleton 0 in
  let r2 = Raz2.empty (rnd_level())
    |> Raz2.do_cmd (Raz2.Insert(Raz2.L,0,rnd_level()))
  in
  let ft = F.singleton 0 in
  let seq = Seq.singleton 0 in

  (* Printf.printf "after init\n"; *)

  (* makes a list of (position, value) pairs for inserting integers *)
  let gen_insertions initial_size total_insertions =
    let rec gen current_size total l =
      if total <= 0 then l else
      let rand = (Random.int current_size,current_size) in
      gen (current_size + 1) (total - 1) (rand::l)

    in
    List.rev (gen (initial_size + 1) total_insertions [])
  in


  (* gen common test data *)
  Random.init Params.rnd_seed;
  let inserts = gen_insertions 0 10 in

  (* run tests *)
  let seq = insert_seq inserts seq in
  let ft = insert_ft inserts ft in
  let r = insert_r inserts r in
  let r2 = insert_r2 inserts r2 in

  (* Print results *)
  (* TODO: compare results *)
  Printf.printf "Sequence(baseline): \n";
  List.iter (Printf.printf "%d; ") (to_list_seq seq);
  Printf.printf "\n";

  Printf.printf "Fingertree: \n";
  List.iter (Printf.printf "%d; ") (to_list_ft ft);
  Printf.printf "\n";

  Printf.printf "Raz_simp: \n";
  List.iter (Printf.printf "%d; ") (to_list_r r);
  Printf.printf "\n";

  Printf.printf "Raz2: \n";
  List.iter (Printf.printf "%d; ") (to_list_r2 r2);
  Printf.printf "\n"

let _ = test()
