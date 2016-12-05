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
  type seq = list
  let insert val index seq =
    let with_index = List.mapi (fun i a -> (i,a)) seq in
    let (left,right) = List.partition (fun (i,_) -> i < index) with_index in
    let with_val = left @ ((0,val)::right) in
    let without_index = List.map (fun (_,a) -> a)) with_val in
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
  let size = f.size ft in
  let rec drain ft l size =
    match size with
    | 0 -> l
    | _ ->
    let l = (F.last ft)::l in
    let r = F.init ft in
    drain r l (size - 1)
  in
  drain ft size []

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
  drain r size []

let to_list_r2 r2 =
  let t = Raz2.unfocus r2 in
  let size = Raz2.elm_cnt t in
  let r2 = Raz2.focus t size in
  let rec drain r l size =
    match size with
    | 0 -> l
    | _ ->
    (* TODO: RAZ2 getlast *)
    let l = (Raz.view Raz.L r)::l in
    let r = Raz.remove Raz.L r in
    drain r l (size - 1)
  in
  drain r size []
  

let rec rnd_insert_seq current_size n seq =
  if n <= 0 then seq else
  let p = Random.int (current_size + 1) in
  let seq = Seq.insert n p seq in
  rnd_insert_seq (current_size + 1) (n - 1) seq

let rec rnd_insert_ft current_size n ft =
  if n <= 0 then ft else
  let p = Random.int (current_size+1) in
    let left, right = F.split_at ft p in
    let ft = F.append (F.snoc left n) right in
  rnd_insert_ft (current_size+1) (n-1) ft

let rec rnd_insert_r current_size n r =
  if n <= 0 then r else
  let p = Random.int (current_size+1) in
  let t = Raz.unfocus r in
  let r = Raz.focus t p in
  let r = Raz.insert Raz.L n r in
  rnd_insert_r (current_size+1) (n-1) r

let rec rnd_insert_r2 current_size n r2 =
  if n <= 0 then r2 else
  let lev = rnd_level() in
  let p  = Random.int (current_size+1) in
  let t  = Raz2.unfocus r2 in
  let r2 = Raz2.focus t p in
  let r2 = Raz2.insert Raz2.L n lev r2 in
  rnd_insert_r2 (current_size+1) (n-1) r2

let test() =
  (* init seqs *)
  let r = Raz.singleton 0 |> Raz.insert Raz.L 0 in
  let r2 = Raz2.empty (rnd_level())
    |> Raz2.do_cmd (Insert(Raz2.L,0,rnd_level()))
    |> Raz2.do_cmd (Insert(Raz2.L,0,rnd_level()))
  in
  let ft = F.snoc (F.singleton 0) 0 in

  (* Printf.printf "after init\n"; *)

  (* init random generator *)
  Random.init Params.rnd_seed;

  (* run tests *)

let _ = test()
