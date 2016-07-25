type lev = int
type cnt = int
type dir = L | R

type 'a tree =
| Nil
| Leaf of 'a
| Bin  of lev * cnt * 'a tree * 'a tree

type 'a tlist =
| Nil
| Cons of 'a * 'a tlist
| Level of lev * 'a tlist
| Tree of 'a tree * 'a tlist

type 'a raz = ('a tlist * 'a * 'a tlist)

(* neg binomial dist *)
let rnd_level : unit -> lev =
  fun () ->
  let rec ffs x =
    if x = 0 then 0 else
    let rec loop t r =
      if (x land t) = 0 then r
      else loop (t lsl 1) (r + 1)
    in loop 1 0
  in
  ffs (Random.bits())

let singleton e = (Nil,e,Nil)
let empty n = (Level(rnd_level(),Cons(n,Nil)),n,Nil)

let item_count : 'a tree -> cnt =
  fun t -> match t with
  | Nil -> 0
  | Leaf(_) -> 1
  | Bin(_,c,_,_) -> c

(* if the first item of tlist is a tree, break down away from the dir side *)
let trim : dir -> 'a tlist -> 'a tlist =
  fun d tl ->
  match tl with
  | Nil | Cons _ | Level _ -> tl
  | Tree(t, rest) ->
    let rec trim : 'a tree -> 'a tlist -> 'a tlist =
      fun h1 t1 -> match h1 with
      | Nil -> failwith "poorly formed tree"
      | Leaf(elm) -> Cons(elm,t1)
      | Bin(lv,_,l,r) -> match d with
        | L -> trim r (Level(lv,Tree(l,t1)))
        | R -> trim l (Level(lv,Tree(r,t1)))
    in trim t rest

let view_c : 'a raz -> 'a =
  fun (_,e,_) -> e
let view : dir -> 'a raz -> 'a =
  let rec view d s = match s with
  | Nil -> failwith "view past end of seq"
  | Cons(e,_) -> e
  | Level(_,rest) -> view d rest
  | Tree _ -> view d (trim d s)
  in fun d (l,e,r) -> match d with
  | L -> view L l | R -> view R r

let alter_c :  'a -> 'a raz ->'a raz =
  fun e (l,_,r) -> (l,e,r)
let alter : dir -> 'a -> 'a raz -> 'a raz =
  let rec alter e d s = match s with
  | Nil -> failwith "alter past end of seq"
  | Cons(_,rest) -> Cons(e,rest)
  | Level(lv,rest) -> Level(lv,alter e d rest)
  | Tree _ -> alter e d (trim d s)
  in fun d elm (l,e,r) -> match d with
  | L -> (alter elm L l,e,r)
  | R -> (l,e,alter elm R r)

let insert :  dir -> 'a -> 'a raz -> 'a raz =
  fun d ne (l,e,r) -> match d with
  | L -> (Level(rnd_level(),Cons(ne,l)),e,r)
  | R -> (l,e,Level(rnd_level(),Cons(ne,r)))

let remove : dir -> 'a raz -> 'a raz =
  let rec remove d s = match s with
  | Nil -> failwith "remove past end of seq"
  | Cons(_,rest) -> rest
  | Level(lv,rest) -> remove d rest
  | Tree _ -> remove d (trim d s)
  in fun d (l,e,r) -> match d with
  | L -> (remove L l,e,r)
  | R -> (l,e,remove R r)

let move : dir -> 'a raz -> 'a raz =
  let rec move d f s = match f with
  | Nil -> failwith "move past end of seq"
  | Cons(elm, rest) -> (rest,elm,s)
  | Level(lv,rest) ->
    move d rest (Level(lv,s))
  | Tree _ -> move d (trim d f) s
  in fun d (l,e,r) -> match d with
  | L -> move L l (Cons(e,r)) 
  | R -> let (r,e,l) =
    move R r (Cons(e,l))
  in (l,e,r)

(* focus through tree to position or one end, using accumulator zipper *)
let rec focus : 'a tree -> int -> 'a raz =
  fun t p -> (* first a top-level bounds check *)
  let c = item_count t in
  if p >= c then focus t (c - 1) else
  if p < 0 then focus t 0 else
  let rec focus : 'a tree -> int -> ('a tlist * 'a tlist) -> 'a raz =
    fun t p (l,r) -> match t with
    | Nil -> failwith "internal Nil"
    | Leaf(elm) -> 
      assert (p == 0);
      (l,elm,r)
    | Bin(lv, _, bl, br) -> 
      let c = item_count bl in
      if p < c
      then focus bl p (l,Level(lv,Tree(br,r)))
      else focus br (p - c) (Level(lv,Tree(bl,l)),r)
  in focus t p (Nil,Nil)

let rec join_sides : 'a tree -> 'a tree -> 'a tree =
  fun t1 t2 -> 
  let tot = (item_count t1)+(item_count t2) in
  match (t1, t2) with
  | Nil, _ -> t2 | _, Nil -> t1
  | Leaf(_), Leaf(_) ->
    failwith "leaf-leaf: full trees shouldn't be joined"
  | Leaf(_), Bin(lv,_,l,r) ->
    Bin(lv, tot, join_sides t1 l, r)
  | Bin(lv,_,l,r), Leaf(_) ->
    Bin(lv, tot, l, join_sides r t2)
  | Bin(lv1,_,t1l,t1r), Bin(lv2,_,t2l,t2r) ->
    if lv1 >= lv2
    then Bin(lv1, tot, t1l, join_sides t1r t2)
    else Bin(lv2, tot, join_sides t1 t2l, t2r)

let head_as_tree : 'a tlist -> 'a tree =
  fun l -> match l with
  | Nil -> Nil
  | Cons(s,_) -> Leaf(s)
  | Level(l,_) -> 
    Bin(l,(item_count Nil)+
      (item_count Nil),Nil,Nil)
  | Tree(t,_) -> t

let tail : 'a tlist -> 'a tlist =
  fun l -> match l with
  | Nil -> Nil
  | Cons(_,r) | Level(_,r) | Tree(_,r) -> r

(* build up the tlist on its dir side *)
let grow : dir -> 'a tlist -> 'a tree =
  fun d t ->
  let rec grow = fun h1 t1 ->
    match t1 with Nil -> h1 | _ ->
    let h2 = head_as_tree t1 in
    match d with
    | L -> grow (join_sides h2 h1) (tail t1)
    | R -> grow (join_sides h1 h2) (tail t1)  
  in grow (head_as_tree t) (tail t)

let unfocus : 'a raz -> 'a tree =
  fun (l,e,r) -> join_sides (grow L l)
    (join_sides (Leaf(e)) (grow R r))

let print_raz : ('a -> string) -> 'a raz -> unit =
  fun string_of_elm (l,e,r) ->
    let rec list_of_tree : dir -> 'a tree -> 'a list -> 'a list =
    fun d t acc -> match t with
    | Nil -> acc
    | Leaf(e) -> e::acc
    | Bin(_,_,t1,t2) -> match d with
      | L -> acc |> list_of_tree d t2 |> list_of_tree d t1
      | R -> acc |> list_of_tree d t1 |> list_of_tree d t2
    in
    let rec list_of_half : dir -> 'a tlist -> 'a list -> 'a list =
    fun d h acc -> match h with
    | Nil -> acc
    | Cons(e,rest) -> list_of_half d rest (e::acc)
    | Level(_,rest) -> list_of_half d rest acc
    | Tree(t,rest) -> list_of_half d rest (list_of_tree d t acc)
    in
    let stringify acc e = acc ^ (string_of_elm e) ^ "; " in
    let l = list_of_half L l [] in
    let r = List.rev (list_of_half R r []) in
    let acc = List.fold_left stringify "" l in
    let acc = acc ^ ":" ^ (string_of_elm e) ^ ":; " in
    let acc = List.fold_left stringify acc r in
    print_endline acc;

