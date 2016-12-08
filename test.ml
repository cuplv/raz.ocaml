(* 
This file is mostly tests of correctness.
*)

open Zipper
module SQ = Seq
module FT = FingerTree
module RS = RazSimple
module RF = RazFormal

(* TODO: test both directions for all ops *)

module Params = struct
  let rnd_seed_ = ref 0         (* seed value for random number generation *)
  let insert_ = ref 1000
  let replace_ = ref 100
  let observe_ = ref 100
  let remove_ = ref 500

  let args = [
    ("--seed",    Arg.Set_int rnd_seed_,  " random seed");
    ("--insert", Arg.Set_int insert_,   " number of insertions");
    ("--replace", Arg.Set_int insert_,   " number of replacements (after a move)");
    ("--observe", Arg.Set_int remove_,   " number of observations");
    ("--remove", Arg.Set_int remove_,   " number of removals");
  ]

  let _ = Arg.parse args
    (fun arg -> invalid_arg ("Unknown: "^arg))
    "usage: eval [options]"

  let rnd_seed = !rnd_seed_
  let insert = !insert_
  let replace = !replace_
  let remove = !remove_
  let observe = !observe_
end

(* the following generate lists of random commands, taking into account current sequence length *)

(* makes a list of (position, value) pairs for inserting integers *)
let gen_insertions initial_size total_insertions =
  let rec gen current_size total l =
    if total <= 0 then l else
    let rnd_dir = if Random.bool() then L else R in
    let rnd_pos = Random.int current_size in
    let rnd_val = current_size in (* this doesn't need to be random, so it's informative *)
    let ins = (rnd_pos,rnd_dir,rnd_val) in
    gen (current_size + 1) (total - 1) (ins::l)
  in
  List.rev (gen (initial_size + 1) total_insertions [])

(* makes a list of (position, direction bool, moves, value) to move to and replace *)
let gen_replaces initial_size total_replaces =
  []

(* makes a list of positions to look at *)
let gen_observes initial_size total_observes =
  []

(* makes a list of positions for removing values *)
let gen_removals initial_size total_removals =
  let rec gen current_size total l =
    if total <= 0 then l else
    let rand = Random.int current_size in
    gen (current_size - 1) (total - 1) (rand::l)
  in
  List.rev (gen initial_size total_removals [])

let testfailure reason =
  Printf.printf reason;
  Printf.printf "\n";
  Printf.printf "  params were: --seed %d --insert %d --replace %d --remove %d\n --observe %d\n"
    Params.rnd_seed Params.insert Params.replace Params.remove Params.observe

let test() =

  (* init seqs *)
  let r = RS.singleton 0 in
  let r2 = RF.singleton 0 in
  let ft = FT.singleton 0 in
  let seq = SQ.singleton 0 in

  (* gen common test data *)
  Random.init Params.rnd_seed;
  let inserts = gen_insertions 0 Params.insert in
  let move_replaces = gen_replaces Params.insert Params.replace in
  let observes = gen_observes (Params.insert - Params.remove) Params.observe in
  let removals = gen_removals Params.insert Params.remove in

  (* run insertion tests *)
  let seq = List.fold_left (fun s (p,d,v) -> SQ.unfocus s |> SQ.focus p |> SQ.insert d v) seq inserts in 
  let ft = List.fold_left (fun s (p,d,v) -> FT.unfocus s |> FT.focus p |> FT.insert d v) ft inserts in
  let r = List.fold_left (fun s (p,d,v) -> RS.unfocus s |> RS.focus p |> RS.insert d v) r inserts in
  let r2 = List.fold_left (fun s (p,d,v) -> RF.unfocus s |> RF.focus p |> RF.insert d v) r2 inserts in

  (* convert to lists *)
  let seql = SQ.list_of_zipper (SQ.unfocus seq) in
  let ftl = FT.list_of_zipper (FT.unfocus ft) in
  let rl = RS.list_of_zipper (RS.unfocus r) in
  let r2l = RF.list_of_zipper (RF.unfocus r2) in

  (* check correctness *)
  if seql <> ftl then testfailure "Fingertree failied the insertion step" else
  if seql <> rl then testfailure "Raz_simp failed the insertion step" else
  if seql <> r2l then testfailure "Raz2 failed the insertion step" else

(*
  List.iter (Printf.printf "%d;") seql;
  Printf.printf "\n";
*)

  Printf.printf "Success\n"
  
let _ = test()
