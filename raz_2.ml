(* This version of the RAZ places levels, not elements, at the center
   of the zipper's cursor *)

type lev      = int
type cnt      = int
type dir      = L | R
type bin_info = { lev:lev; elm_cnt:cnt }
type 'a tree  = Bin  of bin_info * 'a tree * 'a tree
	      | Leaf of 'a
	      | Nil
type 'a tlist = Cons of 'a      * 'a llist (* Invariant: Cons always followed by a level *)
	      | Tree of 'a tree * 'a llist (* Invariant: Tree always followed by a level; tree does not use Nil before level. *)
	      | Nil
 and 'a llist = Lev of lev      * 'a tlist
type 'a zip   = { left:'a tlist; lev:lev; right:'a tlist}

let empty (l:lev) : 'a zip = 
  {left=Nil;lev=l;right=Nil}

let tree_of_lev (l:lev) : 'a tree = 
  Bin({lev=l;elm_cnt=0},Nil,Nil)

let elm_cnt_of_tree (t:'a tree) : cnt = 
  match t with
  | Bin(bi,_,_) -> bi.elm_cnt
  | Leaf(_)     -> 1
  | Nil         -> 0

let trim (d:dir) (t:'a tlist) : ('a * 'a llist) option =
  match t with
  | Nil -> None
  | Cons(a, llist) -> Some((a, llist))
  | Tree(t, llist) -> 
     let rec loop (t:'a tree) (l:'a llist) : ('a * 'a llist) option =
       match t with
       | Nil                  -> failwith "poorly formed tree"
       | Leaf(a)              -> Some((a, llist))
       | Bin(bi, left, right) -> 
	  match d with
	  | L -> loop left  (Lev(bi.lev, Tree(right, l)))
	  | R -> loop right (Lev(bi.lev, Tree(left,  l)))
     in loop t llist
		       
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
     | L -> fun z -> {z with left  = Cons(a, Lev(lev, z.left ))}
     | R -> fun z -> {z with right = Cons(a, Lev(lev, z.right))}
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
    | Some((elm, Lev(lev, rest))) -> (
      match cmd with 
      | Insert _ -> failwith "impossible"
      | Remove(_) -> 
	 (match d with L -> {z with left =rest} 
		     | R -> {z with right=rest})
      | Replace(_, a) -> 
	 (match d with L -> {z with left =Cons(a, Lev(lev, rest))}
		     | R -> {z with right=Cons(a, Lev(lev, rest))})
      | Move(_) -> 
	 (match d with L -> {left =rest; lev=lev; right=Cons(elm,Lev(z.lev,z.right))}
		     | R -> {right=rest; lev=lev; left =Cons(elm,Lev(z.lev,z.left ))})))

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

let tlist_hd : 'a tlist -> 'a tree = function
  | Nil       -> Nil
  | Cons(s,_) -> Leaf(s)
  | Tree(t,_) -> t

let tlist_tl : 'a tlist -> lev * 'a tlist = function
  | Nil -> failwith "illegal argument"
  | Cons(_,Lev(lev,r)) | Tree(_,Lev(lev,r)) -> (lev, r)

let append_all (d:dir) (trees:'a tlist) : 'a tree =
  let rec loop (tree:'a tree) : lev * 'a tlist -> 'a tree = function
    | (lev, Nil)   -> tree
    | (lev, trees) -> 
       let tree2 = tlist_hd trees in 
       match d with
       | L -> loop (append tree2 (append (tree_of_lev lev) tree)) (tlist_tl trees)
       | R -> loop (append (append tree (tree_of_lev lev)) tree2) (tlist_tl trees)
  in loop (tlist_hd trees) (tlist_tl trees)

let unfocus (z: 'a zip) : 'a tree = 
  append (append_all L z.left) 
	 (append (tree_of_lev z.lev) (append_all R z.right))

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
