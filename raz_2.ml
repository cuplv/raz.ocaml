
type lev      = int
type cnt      = int
type dir      = L | R
type bin_info = { lev:lev; elm_cnt:cnt }
type 'a tree  = Bin  of bin_info * 'a tree * 'a tree
	      | Leaf of 'a
	      | Nil
type 'a tlist = Cons of 'a      * 'a llist
	      | Tree of 'a tree * 'a llist
	      | Nil
 and 'a llist = Lev of lev      * 'a tlist
    	      | LevNil
type 'a zip   = { left:'a tlist; lev:lev; right:'a tlist}

let empty (l:lev) : 'a zip = {left=Nil;lev=l;right=Nil}
let singleton (e:'a) (l:lev) (d:dir) : 'a zip = 
  match d with
  | L -> {left=Cons(e,LevNil);lev=l;right=Nil}
  | R -> {left=Nil;lev=l;right=Cons(e,LevNil)}

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
  | Remove (d) -> (
    match d with
    | L -> ( 
      fun z -> match trim L z.left with
	       | None -> z
	       | Some((_, llist)) -> match llist with 
				     | LevNil -> {z with left=Nil}
				     | Lev(_,l) -> {z with left=l}
    )
    | R -> (
      fun z -> match trim R z.right with
	       | None -> z
	       | Some((_, rlist)) -> match rlist with 
				     | LevNil -> {z with right=Nil}
				     | Lev(_,r) -> {z with right=r}
    )
  )
  | Replace (d,a) -> failwith "TODO"
  | Move (d) -> failwith "TODO"

