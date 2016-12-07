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

(* inefficient but obviously correct sequence implementation to use as baseline *)
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

type elm = int
[@@deriving show]

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
  (* let _ = Format.printf "pre-unfocus: r2=%a\n%!" (Raz2.pp_zip pp_elm) r2 in *)
  let t = Raz2.unfocus r2 in
  (* let _ = Format.printf "pre-focus: t=%a\n%!" (Raz2.pp_tree pp_elm) t in *)
  let size = Raz2.elm_cnt t in
  let r2 = Raz2.focus t size in
  let rec drain r2 l =
    (* let _ = Format.printf "pre-peek: r2=%a\n%!" (Raz2.pp_zip pp_elm) r2 in *)
    match Raz2.peek Raz2.L r2 with
    | None -> l
    | Some(e) ->
    let l = e::l in
    (* let _ = Format.printf "pre-remove: r2=%a\n%!" (Raz2.pp_zip pp_elm) r2 in *)
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

(* TODO: create move-replace code *)
let move_replace_seq replaces seq =
  seq

let move_replace_ft replaces ft =
  ft

let move_replace_r replaces r =
  r

let move_replace_r2 replaces r2 =
  r2

(* TODO: create observation code (returns list of observations) *)
let observe_seq observes seq =
  []

let observe_ft observes ft =
  []

let observe_r observes r =
  []

let observe_r2 observes r2 =
  []

(* TODO: create removal code *)
let remove_seq removals seq =
  seq

let remove_ft removals ft =
  ft

let remove_r removals r =
  r

let remove_r2 removals r2 =
  r2


(* the following generate lists of random commands, taking into account current sequence length *)

(* makes a list of (position, value) pairs for inserting integers *)
let gen_insertions initial_size total_insertions =
  let rec gen current_size total l =
    if total <= 0 then l else
    let rand = (Random.int current_size,current_size) in
    gen (current_size + 1) (total - 1) (rand::l)
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

  (* TODO: change tests to folds over more common single operations *)

  (* init seqs *)
  let r = Raz.singleton 0 in
  let r2 = Raz2.empty (rnd_level())
    |> Raz2.do_cmd (Raz2.Insert(Raz2.L,0,rnd_level()))
  in
  let ft = F.singleton 0 in
  let seq = Seq.singleton 0 in

  (* gen common test data *)
  Random.init Params.rnd_seed;
  let inserts = gen_insertions 0 Params.insert in
  let move_replaces = gen_replaces Params.insert Params.replace in
  let observes = gen_observes (Params.insert - Params.remove) Params.observe in
  let removals = gen_removals Params.insert Params.remove in

  (* run insertion tests *)
  let seq = insert_seq inserts seq in
  let ft = insert_ft inserts ft in
  let r = insert_r inserts r in
  let r2 = insert_r2 inserts r2 in

  (* convert to lists *)
  let seql = to_list_seq seq in
  let ftl = to_list_ft ft in
  let rl = to_list_r r in
  let r2l = to_list_r2 r2 in

  (* check correctness *)
  if seql <> ftl then testfailure "Fingertree failied the insertion step" else
  if seql <> rl then testfailure "Raz_simp failed the insertion step" else
  if seql <> r2l then testfailure "Raz2 failed the insertion step" else
  
  (* run move-replace tests *)
  let seq = move_replace_seq move_replaces seq in
  let ft = move_replace_ft move_replaces ft in
  let r = move_replace_r move_replaces r in
  let r2 = move_replace_r2 move_replaces r2 in

  (* convert to lists *)
  let seql = to_list_seq seq in
  let ftl = to_list_ft ft in
  let rl = to_list_r r in
  let r2l = to_list_r2 r2 in

  (* check correctness *)
  if seql <> ftl then testfailure "Fingertree failied the move-replace step" else
  if seql <> rl then testfailure "Raz_simp failed the move-replace step" else
  if seql <> r2l then testfailure "Raz2 failed the move-replace step" else

  (* run observation tests *)
  let seq_o = observe_seq observes seq in
  let ft_o = observe_ft observes ft in
  let r_o = observe_r observes r in
  let r2_o = observe_r2 observes r2 in

  (* check correctness *)
  if seq_o <> ft_o then testfailure "Fingertree failied the observe step" else
  if seq_o <> r_o then testfailure "Raz_simp failed the observe step" else
  if seq_o <> r2_o then testfailure "Raz2 failed the observe step" else

  (* run removal tests *)
  let seq = remove_seq removals seq in
  let ft = remove_ft removals ft in
  let r = remove_r removals r in
  let r2 = remove_r2 removals r2 in

  (* convert to lists *)
  let seql = to_list_seq seq in
  let ftl = to_list_ft ft in
  let rl = to_list_r r in
  let r2l = to_list_r2 r2 in

  (* check correctness *)
  if seql <> ftl then testfailure "Fingertree failied the removal step" else
  if seql <> rl then testfailure "Raz_simp failed the removal step" else
  if seql <> r2l then testfailure "Raz2 failed the removal step" else

  Printf.printf "Success\n"
  
let _ = test()
