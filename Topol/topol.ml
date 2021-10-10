(* Mikołaj Uzarski gr. 5 *)
(* code review: Wiktor Chmielewski gr. 5 *)

open PMap;;

(* wyjątek rzucany, gdy zależności w grafie są cykliczne *)
exception Cykliczne;;

(* trzy stany wierzcholka *)
type state = Unvisited | Processing | Visited;;

(* konwersja listy sąsiedztwa w mapę *)
let toMap l =
  let f acc (v, l) =
    let (st, lst) = try find v acc with Not_found -> (ref Unvisited, []) in
    add v (st, lst @ l) acc
  in List.fold_left f empty l;;

let topol l =
  let gr = ref (toMap l) (* referencja do mapy *)
  and res = ref [] in (* wynikowa lista *)
  let rec visit v (st, l) =
    if !st = Visited then () (* jak odwiedzilismy - to nic nie robimy *)
    else if !st = Processing then raise Cykliczne (* w trakcie odwiedzania - mamy cykl - rzucamy wyjątek *)
    else begin
      st := Processing;
      List.iter (fun x ->
        if mem x !gr then visit x (find x !gr)
        else begin (* mamy wierzcholek, od którego nie wychodzą krawędzie - dodajemy do listy *)
          res := x :: (!res);
          gr := add x (ref Visited, []) !gr;
        end) l;
      st := Visited;
      res := v :: (!res); (* skonczylismy przeglądać wierzchołek - dodajemy do listy wynikowej *)
    end
  in iter visit !gr; !res;;