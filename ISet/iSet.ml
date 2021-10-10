(* Mikołaj Uzarski gr. 5 *)
(* code review: Julia Sobiech gr. 5 *)

(* dla (x,y) zakladamy x <= y *)
type interval = int * int;;

(* lewy syn, wartosci, prawy syn, wysokosc, ilosc elementow w drzewie *)
type t =
  | Empty
  | Node of t * interval * t * int * int;;

(* zwraca wysokosc drzewa *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0;;

(* zwraca liczbę elementow w drzewie *)
let cardinality = function
  | Node (_, _, _, _, x) -> x
  | Empty -> 0;;

(* jako ze ilosc elementow - a - musi byc dodatnia *)
(* to gdy jest ujemna to znaczy, ze przekroczyla max_int *)
let to_max_int a =
  if a < 0 then max_int else a;;

(* tworzy drzewo o wartosci (x,y) i poddrzewach l r *)
let make l (x, y) r = Node (l, (x, y), r, max (height l) (height r) + 1,
   to_max_int ((cardinality l) + (cardinality r) + (y - x + 1)));;

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r;;

(* zlozonosc czasowa: O(log n) *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found;;

(* zwraca drzewo otrzymane po usunieciu najmniejszego elementu *)
(* zlozonosc czasowa : O(log n) *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt";;

(* laczy dwa drzewa t1 i t2 *)
(* zaklada, ze wartosci t1 < wartosci t2 *)
(* t1 i t2 spelniaja warunki drzewa avl *)
(* zlozonosc czasowa : O(log n) *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2);;

let empty = Empty;;

let is_empty x = x = Empty;;

(* dodaje przedzial (x, y) do drzewa z zalozeniem, ze *)
(* dla dowolnych przedzialow (a, b) z drzewa zachodzi *)
(* (y < a - 1) || (x > b + 1) *)
(* zlozonosc czasowa : O(log n) *)
let rec add_aux (x, y) t =
  match t with
    | Empty -> make Empty (x, y) Empty
    | Node (l, (a, b), r, _, _) ->
      if x > b then
        let nr = add_aux (x, y) r in
          bal l (a, b) nr
      else let nl = add_aux (x, y) l in
          bal nl (a, b) r;;

(* laczy dwa drzewa l i r, oraz dodaje do nich przedzial v *)
(* zaklada, ze wartosci l < v < wartosci r *)
(* dodatkowo upewnia sie zeby powstale drzewo spelnialo warunki drzewa avl *)
(* zlozonosc czasowa : O(abs ((wysokosc l) - (wysokosc r))) *)
let rec join l x r =
  match (l, r) with
  | (Empty, _) -> add_aux x r
  | (_, Empty) -> add_aux x l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr x r) else
      if rh > lh + 2 then bal (join l x rl) rv rr else
      make l x r;;

let split x t =
  let rec aux x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, (a, b), r, _, _) ->
      if x < a then
        let (ll, pres, rl) = aux x l in (ll, pres, join rl (a, b) r)
      else if x > b then
        let (lr, pres, rr) = aux x r in (join l (a, b) lr, pres, rr)
      else
        let ll =
          if x > a then join l (a, x - 1) Empty
          else l
        and rr =
          if x < b then join Empty (x + 1, b) r
          else r
      in (ll, true, rr)
  in aux x t;;

(* dzieli drzewo na dwa drzewa (<x) i (>y) po czym je laczy *)
let remove (x, y) t =
  let l = (fun (a, b, c) -> a) (split x t)
  and r = (fun (a, b, c) -> c) (split y t)
    in merge l r;;

(* szuka przedzialu takiego, ze x in (a, b) i zwraca a *)
(* jak nie znajdzie zwraca x + 1 *)
(* zlozonosc czasowa : O(log n) *)
let rec get_left x t =
  match t with
    | Empty -> x + 1
    | Node (l, (a, b), r, _, _) ->
      if x < a then get_left x l
      else if x > b then get_left x r
      else a;;

(* szuka przedzialu takiego, ze x in (a, b) i zwraca b *)
(* jak nie znajdzie zwraca x - 1 *)
(* zlozonosc czasowa : O(log n) *)
let rec get_right x t =
  match t with
    | Empty -> x - 1
    | Node (l, (a, b), r, _, _) ->
      if x < a then get_right x l
      else if x > b then get_right x r
      else b;;

(* laczy przedzialy nierozlaczne z przedzialem (x,y) i skleja je w jeden przedzial *)
(* przed dodaniem usuwa caly przedzial *)
(* get_right, get_left, remove oraz add_aux maja zlozonosc O(log n) *)
(* wywolujemy je raz, wobec tego dostajemy c * log n, gdzie c to stala *)
(* zlozonosc czasowa : O(log n) *)
let add (x, y) t =
  let mergedInterval =
    if x = min_int && y = max_int then (x, y)
    else if x = min_int then (x, get_right (y + 1) t)
    else if y = max_int then (get_left (x - 1) t, y)
    else (get_left (x - 1) t, get_right (y + 1) t) in
  add_aux mergedInterval (remove mergedInterval t);;

let rec mem x = function
  | Empty -> false
  | Node (l, (a, b), r, _, _) ->
    if x >= a && x <= b then true
    else if x < a then mem x l
    else mem x r;;

let elements t =
  let rec aux a t =
  match t with
    | Empty -> a
    | Node (l, i, r, _, _) ->
      aux (i :: (aux a r)) l
  in aux [] t;;

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t;;

let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t;;

let below x t =
  let rec aux acc t =
    match t with
      | Empty -> acc
      | Node (l, (a, b), r, _, _) ->
        if b = max_int && a = min_int && x > 0 then max_int
        else
          if x > b then aux (acc + cardinality l + b - a + 1) r
          else if x < a then aux acc l
        else aux (acc + x - a + 1) l
  in let fix = aux 0 t in
    if fix < 0 then max_int else fix;;