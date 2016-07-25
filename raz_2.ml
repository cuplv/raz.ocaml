(* This version of the RAZ places levels, not elements, at the center
   of the zipper's cursor *)

type lev      = int
type cnt      = int
type dir      = L | R
type bin_info = { lev:lev; elm_cnt:cnt }
type 'a tree  = Bin  of bin_info * 'a tree * 'a tree
	      | Leaf of 'a
	      | Nil
type 'a elms  = Cons of 'a * lev * 'a elms (* Invariant: element always followed by a level *)
	      | Trees of ('a tree) list    (* Invariant: trees not interposed with elements/levels *)
type 'a zip   = { left:'a elms; lev:lev; right:'a elms}

let empty (l:lev) : 'a zip = 
  {left=Trees([]);lev=l;right=Trees([])}

let tree_of_lev (l:lev) : 'a tree = 
  Bin({lev=l;elm_cnt=0},Nil,Nil)

let elm_cnt_of_tree (t:'a tree) : cnt = 
  match t with
  | Bin(bi,_,_) -> bi.elm_cnt
  | Leaf(_)     -> 1
  | Nil         -> 0

let trim (d:dir) (t:'a elms) : ('a * lev * 'a elms) option =
  match t with
  | Cons(a, lev, elms) -> Some((a, lev, elms))
  | Trees([])          -> None
  | Trees(tree::trees) -> 
     let rec loop (t:'a tree) (lo:lev option) (trees:('a tree)list ) 
	     : ('a * lev * 'a elms) option =
       match t, lo with
       | Nil, _                  -> failwith "poorly formed tree"
       | Leaf(_), None           -> failwith "poorly formed tree"
       | Leaf(a), Some(lev)      -> Some((a, lev, Trees(trees)))
       | Bin(bi, left, right), _ -> 
	  let trees = match lo with None      -> trees
				  | Some(lev) -> (tree_of_lev lev) :: trees
	  in
	  match d with
	  | L -> loop left  (Some bi.lev) (right :: trees)
	  | R -> loop right (Some bi.lev) (left  :: trees)
     in loop tree None trees
		       
type 'a zip_cmd =
  | Insert  of dir * 'a * lev
  | Remove  of dir
  | Replace of dir * 'a
  | Move    of dir
type 'a zip_cmds = 'a zip -> 'a zip

let do_zip_cmd : 'a zip_cmd -> 'a zip_cmds =
  function
  | Insert (d,a,lev) -> (
     match d with
     | L -> fun z -> {z with left  = Cons(a, lev, z.left )}
     | R -> fun z -> {z with right = Cons(a, lev, z.right)}
  )
  | ( Remove (d)
    | Replace(d,_)
    | Move   (d) ) as cmd -> (
    fun z ->
    let trimmed = match d with
      | L -> trim L z.left
      | R -> trim L z.right
    in
    match trimmed with
    | None -> z (* do nothing *)
    | Some((elm, lev, rest)) -> (
      match cmd with
      | Insert _ -> failwith "impossible"
      | Remove(_) ->
	 (match d with L -> {z with left =rest}
		     | R -> {z with right=rest})
      | Replace(_, a) ->
	 (match d with L -> {z with left =Cons(a, lev, rest)}
		     | R -> {z with right=Cons(a, lev, rest)})
      | Move(_) ->
	 (match d with L -> {left =rest; lev=lev; right=Cons(elm,z.lev,z.right)}
		     | R -> {right=rest; lev=lev; left =Cons(elm,z.lev,z.left )})))

let rec append (t1:'a tree) (t2:'a tree) : 'a tree =
  let elm_cnt = (elm_cnt_of_tree t1) + (elm_cnt_of_tree t2) in
  match t1, t2 with
  | Nil, _ -> t2
  | _, Nil -> t1
  | Leaf(_), Leaf(_)       -> failwith "illegal argument: Leaf-Leaf case not handled"
  | Leaf(a), Bin(bi, l, r) -> Bin({lev=bi.lev;elm_cnt=elm_cnt}, append t1 l, r)
  | Bin(bi, l, r), Leaf(a) -> Bin({lev=bi.lev;elm_cnt=elm_cnt}, l, append r t2)
  | Bin(bi1, l1, r1),
    Bin(bi2, l2, r2) -> if bi1.lev >= bi2.lev
			then Bin({lev=bi1.lev;elm_cnt=elm_cnt}, l1, append r1 t2)
			else Bin({lev=bi2.lev;elm_cnt=elm_cnt}, append t1 l2, r2)

let rec tree_of_trees (d:dir) (tree:'a tree) (trees:('a tree)list) : 'a tree =
  match trees with
  | [] -> tree
  | tree2::trees -> 
     match d with
     | L -> tree_of_trees d (append tree tree2) trees
     | R -> tree_of_trees d (append tree2 tree) trees

let rec tree_of_elms (d:dir) (tree:'a tree) (elms:'a elms) : 'a tree =
  match elms with
  | Trees(trees)       -> tree_of_trees d tree trees
  | Cons(elm,lev,elms) -> 
     match d with
     | L -> tree_of_elms d (append tree (append (Leaf elm) (tree_of_lev lev))) elms
     | R -> tree_of_elms d (append (tree_of_lev lev) (append (Leaf elm) tree)) elms

let unfocus (z: 'a zip) : 'a tree =
  append (tree_of_elms L Nil                 z.left ) 
	 (tree_of_elms R (tree_of_lev z.lev) z.right)

(*
let focus (tree:'a tree) (pos:int) : 'a zip =
  let rec loop (tree:'a tree) (pos:int) (ll:('a llist) option) (lr:('a llist) option) =
    match tree with
    | Bin(bi, tl, tr) -> 
       let cl = elm_cnt_of_tree tl in
       let cons t = function None ->  | Some(l) -> Some(Tree(t, l))

       if      pos = cl then {lev=bi.lev; left=cons tl ll; right=cons tr lr}
       else if pos < cl then loop tl pos        ll (lev_cons bi.lev tr lr)
       else if pos > cl then loop tr (pos - cl) (lev_cons bi.lev tl ll) lr
       else failwith "impossible"
    | _ -> failwith "illegal argument"
  in
  let elm_cnt = elm_cnt_of_tree tree in
  let pos = if      pos >= elm_cnt then elm_cnt
	    else if pos < 0        then 0 
	    else    pos in
  match tree with 
  | Nil     -> failwith "illegal argument"
  | Leaf(_) -> failwith "illegal argument"
  | Bin(a, Nil, Nil)                 -> {left=Nil; lev=a.lev; right=Nil}
  | Bin(a, Bin(b, Nil, Leaf(x)), t3) -> {left=Nil; 
 *)

(* Count valid instances:

0: a
Bin(a, Nil, Nil)

1: a x b
Bin(b, Bin(a, Nil, Leaf(x)), Nil)
Bin(a, Nil, Bin(b, Leaf(x), Nil))

2: a x b y c
Bin(a, Bin(b, Nil, Leaf(x)), Nil)
Bin(a, Nil, Bin(b, Leaf(x), Nil))



 *)

(* let rec focus : 'a tree -> int -> 'a raz = *)
(*   fun t p -> (\* first a top-level bounds check *\) *)
(*   let c = item_count t in *)
(*   if p >= c then focus t (c - 1) else *)
(*   if p < 0 then focus t 0 else *)
(*   let rec focus : 'a tree -> int -> ('a tlist * 'a tlist) -> 'a raz = *)
(*     fun t p (l,r) -> match t with *)
(*     | Nil -> failwith "internal Nil" *)
(*     | Leaf(elm) ->  *)
(*       assert (p == 0); *)
(*       (l,elm,r) *)
(*     | Bin(lv, _, bl, br) ->  *)
(*       let c = item_count bl in *)
(*       if p < c *)
(*       then focus bl p (l,Level(lv,Tree(br,r))) *)
(*       else focus br (p - c) (Level(lv,Tree(bl,l)),r) *)
(*   in focus t p (Nil,Nil) *)
