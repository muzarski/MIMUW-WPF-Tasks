(* Mikołaj Uzarski gr. 5 *)
(* Code reviewer - Andrzej Sijka gr. 1 *)

(* ---------------------------------------------- *)

(* Przedzial(x,y) definiuje zbiór <x, y> *)

(* Dopelnienie(x,y) definiuje zbiór (-infinity, x> U <y, +infinity) *)

(* Pusty definiuje zbiór pusty - dowolna operacja na zbiorze pustym zwraca zbiór pusty, *)
(* poza tym wg skryptu z analizy inf(Pusty) = +infinity, a sup(Pusty) = -infinity *)
 type wartosc = Przedzial of (float * float) | Dopelnienie of (float * float) | Pusty;;

(* ----------------------------------------------- *)

(* Zauważmy, że  *)
(* *operacja* (Przedzial(ax, bx)) (Dopelnienie(ay, by)) mozna przedstawic jako *)
(* ( *operacja* (Przedzial(ax, bx)) (Przedzial(neg_infinity, ay))) U ( *operacja* (Przedzial(ax, bx)) (Przedzial(by, infinity))), gdzie U to suma przedzialow *)

(* Analogicznie można zauważyć, że *operacja* (Dopelnienie(ax,bx)) (Dopelnienie(ay,by)) *)
(* można przedstawić jako sumę przedziałów, które dostaniemy sumując przedziały otrzymane *)
(* poprzez wykonywanie operacji na dwoch parach przedziałów *)

(* Obserwacja ta jest bardzo przydatna przy implementowaniu operacji razy i podzielic, *)
(* ponieważ pozwala uniknąć rozpatrywania wielu przypadków dla przedziałów postaci Dopelnienie *)

(* ----------------------------------------------- *)

(* Funkcje pomocnicze *)

(* Sprawdza czy dana wartość jest nan *)
let is_nan x =
    compare x nan = 0;;

let max x y =
    if x >= y || is_nan x then x else y;;

let min x y =
    if x < y || is_nan x then x else y;;

(* zwraca najwieksza z 4 wartosci *)
let max_of_four a b c d =
    max (max a b) (max c d);;

(* zwraca najmniejsza z 4 wartosci *)
let min_of_four a b c d =
    min (min a b) (min c d);;

(* Sprawdza czy dana wartość jest nan *)
let is_nan x =
    compare x nan = 0;;

(* zwraca sumę dwóch przedziałów *)
(* UWAGA: jako, że w naszym zadaniu możemy dostać maksymalnie sumę dwóch przedziałów rozłącznych *)
(* to pomijamy przypadki gdy z sumy kilku przedziałów dostaniemy sumę więcej niż dwóch przedziałów rozłącznych *)
let rec suma_przedzialow x y =
    match (x, y) with
    |(Przedzial(ax, bx), Przedzial(ay, by)) -> if bx > by then suma_przedzialow (Przedzial(ay,by)) (Przedzial(ax,bx))
                                               else if (bx >= ay) then (if ax < ay then Przedzial(ax, by)
                                                                       else Przedzial(ay, by))
                                               else Dopelnienie(bx, ay)



    |(Przedzial(ax, bx), Dopelnienie(ay, by)) -> if (ax <= ay) && (bx >= by) then Przedzial(neg_infinity, infinity)
                                              else if (ax <= ay ) then Dopelnienie(bx, by)
                                              else if (bx >= by) then Dopelnienie(ay, ax)
                                              else Dopelnienie(ay, by)

    |(Dopelnienie(ax, bx), Przedzial(ay, by)) -> suma_przedzialow (Przedzial(ay, by)) (Dopelnienie(ax, bx))

    |(Dopelnienie(ax, bx), Dopelnienie(ay, by)) -> if (ax >= by) || (bx <= ay) then Przedzial(neg_infinity, infinity)
                                             else if (ax > ay) && (bx < by) then Dopelnienie(ax, bx)
                                             else if (ax > ay) then Dopelnienie(ax, by)
                                             else if (bx < by) then Dopelnienie(ay, bx)
                                             else Dopelnienie(ay, by)

    |(_, _) -> Pusty;;

(* pomaga w mnozeniu -inf/inf i 0 - daje odpowiednio -inf/inf *)
(* UWAGA: przypadek gdy mnożymy przez Przedzia(0.,0.) rozpatrzony jest w funkcji razy *)
let mnoz0 x y =
    match(x, y) with
    |(a,0.) when a = infinity -> infinity
    |(a,0.) when a = neg_infinity -> neg_infinity
    |(0.,b) when b = infinity -> infinity
    |(0.,b) when b = neg_infinity -> neg_infinity
    |(_,_) -> x *. y;;


(* konstruktory *)

let wartosc_dokladnosc x y =
    if x >= 0. then Przedzial ((x -. (x *. y) /. 100.), (x +. (x *. y) /. 100.))
    else Przedzial ((x +. (x *. y) /. 100.), (x -. (x *. y) /. 100.));;

let wartosc_od_do x y =
    Przedzial (x, y);;

let wartosc_dokladna x =
    Przedzial (x, x);;

(* selektory *)

let in_wartosc w x =
    match w with
    Przedzial (a,b) -> (x >= a) && (x <= b)
    | Dopelnienie (a,b) -> (x <= a) || (x >= b)
    | Pusty -> false;;

let min_wartosc x =
    match x with
    Przedzial(a,b) -> a
    | Dopelnienie (a,b) -> neg_infinity
    | Pusty -> nan;;

let max_wartosc x =
    match x with
    Przedzial(a,b) -> b
    | Dopelnienie(_,_) -> infinity
    | Pusty -> nan;;

let sr_wartosc x =
    match x with
    Przedzial(_,_) -> ((min_wartosc x) +. (max_wartosc x)) /. 2.
    | Dopelnienie(_,_) -> nan
    | Pusty -> nan;;

(* modyfikatory *)

let plus x y =
    match (x,y) with

    |(Przedzial(ax,bx), Przedzial(ay, by)) -> Przedzial (ax +. ay, bx +. by)

    |(Przedzial(ax,bx), Dopelnienie(ay, by)) -> if (ay +. bx) >= (by +. ax) then Przedzial(neg_infinity, infinity)
                                             else Dopelnienie ((ay +. bx),(ax +. by))
    |(Dopelnienie(ax,bx), Przedzial(ay, by)) -> if (ax +. by) >= (ay +. bx) then Przedzial(neg_infinity, infinity)
                                             else Dopelnienie((ax +. by),(ay +. bx))
    |(Dopelnienie(ax,bx), Dopelnienie(ay, by)) -> Przedzial(neg_infinity, infinity)

    |(Pusty, _) -> Pusty

    |(_, Pusty) -> Pusty;;

let rec razy x y =
    match(x, y) with

    |(Pusty, _) -> Pusty

    |(_, Pusty) -> Pusty

    |(Przedzial(ax, bx), _) when (ax = 0.) && (bx = 0.) -> Przedzial(0., 0.)

    |(_, Przedzial(ay, by)) when (ay = 0.) && (by = 0.) -> Przedzial(0., 0.)

    (* elementarne operacje na pojedynczych przedziałach *)
    (* na poczatku rozpatrzymy trzy skrajne przypadki z mnożeniem 0 i inf, -inf *)
    |((Przedzial(ax, bx), Przedzial(ay, by))) -> if (ax = neg_infinity) && (bx = infinity) && (ay <> 0.) then Przedzial(neg_infinity, infinity)

                                                 else if (ax = neg_infinity && by = 0.) || (bx = 0. && ay = neg_infinity) then Przedzial(0., infinity)

                                                 else if (ax < 0. && bx = 0. && ay >= 0. && by = infinity) ||
                                                         (ay < 0. && by = 0. && ax >= 0. && bx = infinity) then Przedzial(neg_infinity, 0.)

                                                 else Przedzial((min_of_four (mnoz0 ax ay) (mnoz0 ax by) (mnoz0 bx ay) (mnoz0 bx by)),
                                                                (max_of_four (mnoz0 ax ay) (mnoz0 ax by) (mnoz0 bx ay) (mnoz0 bx by)))

    |(Przedzial(ax, bx), Dopelnienie(ay, by)) -> let p1 = razy (Przedzial(ax, bx)) (Przedzial(neg_infinity, ay))
                                                  and p2 = razy (Przedzial(ax, bx)) (Przedzial(by, infinity))
                                              in
                                              suma_przedzialow p1 p2

    |(Dopelnienie(ax, bx), Przedzial(ay, by)) -> razy (Przedzial (ay, by)) (Dopelnienie(ax, bx))

    |(Dopelnienie(ax, bx), Dopelnienie(ay, by)) -> let p1 = razy (Przedzial(neg_infinity, ax)) (Przedzial(neg_infinity, ay))
                                                 and p2 = razy (Przedzial(neg_infinity, ax)) (Przedzial(by, infinity))
                                                 and p3 = razy (Przedzial(bx, infinity)) (Przedzial(neg_infinity, ay))
                                                 and p4 = razy (Przedzial(bx, infinity)) (Przedzial(by, infinity))
                                             in
                                             suma_przedzialow (suma_przedzialow p1 p2) (suma_przedzialow p3 p4);;

let minus x y =
    plus x (razy (wartosc_dokladna (-1.)) y);;

let rec podzielic x y =
    match (x, y) with

    |(Pusty, _) -> Pusty

    |(_, Pusty) -> Pusty

    |(_, Przedzial(ax, bx)) when ax = 0. && bx = 0. -> Pusty

    (* elementarne operacje na pojedynczych przedziałach *)
    |(Przedzial(ax, bx), Przedzial(ay, by)) -> if (ax = 0.) && (bx = 0.) then Przedzial(0.,0.)

                                               else if (ax >= 0.) && (ay > 0.) then Przedzial((ax /. by), (bx /. ay)) (* ++, ++ *)

                                               else if (ax < 0.) && (bx = 0.) && (ay < 0.) && (by = 0.) then Przedzial(0., infinity) (* -0, -0 *)

                                               else if (ax < 0.) && (bx = 0.) && (ay = 0.) then Przedzial (neg_infinity, 0.) (* -0, 0+ *)

                                               else if (ax = 0.) && (ay < 0.) && (by = 0.) then Przedzial (neg_infinity, 0.) (* 0+, -0 *)

                                               else if (ax >= 0.) && (ay < 0.) && (by = 0.) then Przedzial(neg_infinity, (ax /. ay)) (* ++, -0 *)

                                               else if (ax >= 0.) && (ay = 0.) then Przedzial((ax /. by), infinity) (* ++, 0+ *)

                                               else if (bx < 0.) && (ay < 0.) && (by = 0.) then Przedzial((bx /. ay), infinity) (* --, -0 *)

                                               else if (bx < 0.) && (ay = 0.) then Przedzial(neg_infinity, (bx /. by)) (* --, 0+ *)

                                               else if (ax < 0.) && (bx >= 0.) && (ay = 0.) then Przedzial(neg_infinity, infinity) (* -+, 0+ *)

                                               else if (bx < 0.) && (by < 0.) then Przedzial((bx /. ay), (ax /. by)) (* --, -- *)

                                               else if (ax >= 0.) && (by < 0.) then Przedzial((bx /. by), (ax /. ay)) (* ++, -- *)

                                               else if (bx < 0.) && (ay >= 0.) then Przedzial((ax /. ay), (bx /. by)) (* --, ++ *)

                                               else if (ax >= 0.) && (ay < 0.) && (bx >= 0.) then Dopelnienie((ax /. ay), (ax /. by)) (* ++, -+ *)

                                               else if (ax < 0.) && (bx >= 0.) && (ay >= 0.) then Przedzial((ax /. ay), (bx /. ay)) (* -+, ++ *)

                                               else if (bx < 0.) && (ay < 0.) && (by >= 0.) then Dopelnienie((bx /. by), (bx /. ay))(* --, -+ *)

                                               else if (ax < 0.) && (bx >= 0.) && (by < 0.) then Przedzial((bx /. by), (ax /. by)) (* -+, -- *)

                                               else Przedzial (neg_infinity, infinity) (* -+, -+ *)

    |(Przedzial(ax, bx), Dopelnienie(ay, by)) -> let p1 = podzielic (Przedzial(ax, bx)) (Przedzial(neg_infinity, ay))
                                                  and p2 = podzielic (Przedzial(ax,bx)) (Przedzial(by, infinity))
                                              in
                                              suma_przedzialow p1 p2

    |(Dopelnienie(ax, bx), Przedzial(ay, by)) -> let p1 = podzielic (Przedzial(neg_infinity, ax)) (Przedzial(ay, by))
                                                             and p2 = podzielic (Przedzial(bx, infinity)) (Przedzial(ay, by))
                                              in
                                              suma_przedzialow p1 p2

    |(Dopelnienie(ax, bx), Dopelnienie(ay, by)) -> let p1 = podzielic (Przedzial(neg_infinity, ax)) (Przedzial(neg_infinity, ay))
                                                 and p2 = podzielic (Przedzial(neg_infinity, ax)) (Przedzial(by, infinity))
                                                 and p3 = podzielic (Przedzial(bx, infinity)) (Przedzial(neg_infinity, ay))
                                                 and p4 = podzielic (Przedzial(bx, infinity)) (Przedzial(by, infinity))
                                             in
                                             suma_przedzialow (suma_przedzialow p1 p2) (suma_przedzialow p3 p4);;