(* Mikołaj Uzarski gr 5 *)
(* Code reviewer: Daniel Mastalerz gr.5 *)

(* W niepustym węźle przechowujemy odpowiednio *)
(* priorytet, prawa wysokosc, lewe poddrzewo, prawe poddrzewo *)
(* Null to pusty węzeł *)
type 'a queue = Node of 'a * int * 'a queue * 'a queue | Null;;

exception Empty;;

(* definiuje pustą kolejkę *)
let empty = Null;;

(* sprawdza czy kolejka jest pusta *)
let is_empty t =
    match t with
    Null -> true
    | _ -> false;;

(* łączy dwie kolejki t1 i t2 *)
let join t1 t2 =
    let rec aux t1 t2 =
        match (t1, t2) with
        (Null, Null) -> Null
        |(Null, t) -> t
        |(t, Null) -> t
        |(Node (prio1,pw1,syn1,syn2), Node(prio2,_,_,_)) ->
            if (prio1 > prio2) then aux t2 t1 (* gdy priorytet t1 > priorytet t2 to wywolujemy aux t2 t1 *)
            else let t3 = aux syn2 t2 in (* drzewo t3, które jest wynikiem połączenia prawego poddrzewa i drzewa z mniejszym priorytetem *)
                match syn1, t3 with
                Null, Null -> Node (prio1, 1, Null, Null)
                |Null, t -> Node (prio1, 1, t, Null) (* Null, ma mniejszą prawą wysokość od dowolnego drzewa - doczepiamy Null z prawej strony *)
                |t, Null -> Node (prio1, 1, t, Null)
                |Node (_, pw2, _, _), Node (_, pw3, _, _)-> (* zwracamy drzewo, którego prawym poddrzewem jest drzewo z mniejszą prawą wysokością *)
                    if pw2 < pw3 then Node(prio1, (pw2 + 1), t3, syn1)
                    else Node (prio1, (pw3 + 1), syn1, t3)
    in aux t1 t2;;

(* dodaje element a do kolejki t *)
let add a t =
    join (Node (a, 1, Null, Null)) t;;

(* jesli kolejka jest pusta to rzuca wyjatek, *)
(* w przeciwnym wypadku usuwa element o najwiekszym priorytecie i zwraca ten element wraz z resztą kolejki *)
let delete_min t =
    match t with
    Null -> raise Empty
    |Node(a, pw, syn1, syn2) -> (a, (join syn1 syn2));;