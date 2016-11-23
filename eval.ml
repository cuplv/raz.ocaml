(* 
This file is mostly tests of performance.

RAZ code is all in  'raz_simp.ml'

Fingertree impl is in 'fingertree.ml'
'bat*.ml' files support the fingertree code.
Fingertree and supporting files are from 'https://github.com/ocaml-batteries-team/batteries-included'
*)
module F = Fingertree
module Raz = Raz_simp
module Raz2 = Raz_2.Raz

type elm = int
[@@deriving show]

module Params = struct
  let no_head_ = ref false      (* whether to suppress csv header *)
  let rnd_seed_ = ref 0         (* seed value for random number generation *)
  let tag_ = ref "None"         (* user tag for this testing run *)
  let test_raz_ = ref false     (* whether to test the RAZ *)
  let test_raz2_ = ref false    (* whether to test the alternate RAZ *)
  let test_ft_ = ref false      (* whether to test the Fingertree *)
  let start_ = ref 0            (* starting sequence length *)
  let inserts_ = ref 10000      (* number of insertions at once *)
  let groups_ = ref 10          (* num of insert groups per sequence *)
  let reps_ = ref 1             (* num of times to repeat the process *)
  let mult_inserts_ = ref false (* whether insertions is multiplied by rep number *)

  let args = [
    ("--nohead",  Arg.Set no_head_,       " supress csv header");
    ("--seed",    Arg.Set_int rnd_seed_,  " random seed");
    ("--tag",     Arg.Set_string tag_,    " user tag");
    ("-z",        Arg.Set test_raz_,      " test the RAZ");
    ("-Z",        Arg.Set test_raz2_,     " test the alternate RAZ");
    ("-f",        Arg.Set test_ft_,       " test the Fingertree");
    ("-s",        Arg.Set_int start_,     " starting sequence length");
    ("-i",        Arg.Set_int inserts_,   " number of timed insertions");
    ("-g",        Arg.Set_int groups_,    " insertion groups per sequence");
    ("-r",        Arg.Set_int reps_,      " number of sequences tested");
    ("-m",        Arg.Set mult_inserts_,  " more insertions for each test");
  ]

  let _ = Arg.parse args
    (fun arg -> invalid_arg ("Unknown: "^arg))
    "usage: eval [options]"

  let no_head = !no_head_
  let rnd_seed = !rnd_seed_
  let tag = !tag_
  let test_raz = !test_raz_
  let test_raz2 = !test_raz2_
  let test_ft = !test_ft_
  let start = !start_
  let inserts = !inserts_
  let groups = !groups_
  let reps = !reps_
  let mult_inserts = !mult_inserts_
end

let rnd_level = Raz.rnd_level

let time thunk =
  let start = Unix.gettimeofday () in
  let res = thunk() in
    let stop = Unix.gettimeofday () in
    let t = (stop -. start) in
    (t,res)

let rec rnd_insert_ft current_size n ft =
  if n <= 0 then ft else
  let p = Random.int (current_size+1) in
    let left, right = F.split_at ft p in
    let ft = F.append (F.snoc left n) right in
  rnd_insert_ft (current_size+1) (n-1) ft

let rec rnd_insert_r current_size n r =
  if n <= 0 then r else
  let p = Random.int (current_size+1) in
  let r = Raz.focus (Raz.unfocus r) p in
  let r = Raz.insert Raz.L n r in
  rnd_insert_r (current_size+1) (n-1) r

let rec rnd_insert_r2 current_size n r2 =
  if n <= 0 then r2 else
  let lev = rnd_level() in
  let p = Random.int (current_size+1) in
  let _  = Format.printf "r2: %a@\n" (Raz2.pp_zip pp_elm) r2 in
  
  let t  = Raz2.unfocus r2 in
  let _  = Format.printf "unfocus r2: %a@\n" (Raz2.pp_tree pp_elm) t in  
  
  let r2 = Raz2.focus t p in
  let _  = Format.printf "focus (unfocus r2): %a@\n" (Raz2.pp_zip pp_elm) r2 in
  
  let r2 = Raz2.do_cmd (Raz2.Insert(Raz2.L,n,lev)) r2 in
  let _  = Format.printf "insert (focus (unfocus r2)): %a@\n" (Raz2.pp_zip pp_elm) r2 in

  rnd_insert_r2 (current_size+1) (n-1) r2


let eval() =
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

  (* print csv header *)
  if not Params.no_head then
    Printf.printf "UnixTime,Seed,Tag,SeqType,SeqNum,PriorElements,Insertions,Time\n%!";

  (* initialize seqs with starting items *)
  let r = if Params.test_raz && Params.start > 0 then
    let (t,r) = time (fun()->rnd_insert_r 0 Params.start r) in
    Printf.printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n%!"
      (int_of_float (Unix.time())) Params.rnd_seed Params.tag
      "RAZ" 0 0 Params.start t;
    r
  else r in
  let r2 = if Params.test_raz2 && Params.start > 0 then
    let (t,r2) = time (fun()->rnd_insert_r2 0 Params.start r2) in
    Printf.printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n%!"
      (int_of_float (Unix.time())) Params.rnd_seed Params.tag
      "RAZ-2" 0 0 Params.start t;
    r2
  else r2 in
  let ft = if Params.test_ft && Params.start > 0 then
    let (t,ft) = time (fun()->rnd_insert_ft 0 Params.start ft) in
    Printf.printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n%!"
      (int_of_float (Unix.time())) Params.rnd_seed Params.tag
      "FT" 0 0 Params.start t;
    ft
  else ft in

  (* Printf.printf "after starting vals\n"; *)

  (* run tests *)
  for i = 1 to Params.reps do
    let ins =
      if Params.mult_inserts
      then Params.inserts * i
      else Params.inserts
    in
    (* loop to grow one RAZ sequence *)
    let rec seq_r size repeats r =
        if repeats > 0 then
        let (ins_time,new_r) = time (fun()->rnd_insert_r size ins r) in
        Printf.printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n%!"
          (int_of_float (Unix.time())) Params.rnd_seed Params.tag
          "RAZ" i size ins ins_time;
        seq_r (size + ins) (repeats - 1) new_r
    in
    (* loop to grow one RAZ-2 sequence *)
    let rec seq_r2 size repeats r2 =
        if repeats > 0 then
        let (ins_time,new_r2) = time (fun()->rnd_insert_r2 size ins r2) in
        Printf.printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n%!"
          (int_of_float (Unix.time())) Params.rnd_seed Params.tag
          "RAZ-2" i size ins ins_time;
        seq_r2 (size + ins) (repeats - 1) new_r2
    in
    (* loop to grow one Fingertree sequence *)
    let rec seq_ft size repeats ft =
        if repeats > 0 then
        let (ins_time,new_ft) = time (fun()->rnd_insert_ft size ins ft) in
        Printf.printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n%!"
          (int_of_float (Unix.time())) Params.rnd_seed Params.tag
          "FT" i size ins ins_time;
        seq_ft (size + ins) (repeats - 1) new_ft
    in
    if Params.test_raz then seq_r Params.start Params.groups r;
    if Params.test_raz2 then seq_r2 Params.start Params.groups r2;
    if Params.test_ft then seq_ft Params.start Params.groups ft;
  done

let _ = eval()
