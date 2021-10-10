(* Mikolaj Uzarski gr.5 *)
(* code review : Tomasz Pieszczek gr.5 *)

type point = float * float;;

type kartka = point -> int;;

(* rozwiazuje problem niedokladnosci typu float *)
let epsilon = 1e-14;;

let square x = x *. x;;
let dot_product (x1, y1) (x2, y2) = x1 *. x2 +. y1 *. y2;;
let cross_product (x1, y1) (x2, y2) = x1 *. y2 -. y1 *. x2;;

(* infiksowe mnozenie wektora przez skalar *)
let ( ** ) s (a, b) = (a *. s, b *. s);;
(* infiksowe odejmowanie wektorow *)
let (--) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2);;
(* infiksowe dodawanie wektorow *)
let (++) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2);;

(* odbicie punktu v wzgledem prostej przechodzacej przez (0,0) i punkt l *)
let reflection v l =
  ((2. *. (dot_product v l) /. (dot_product l l)) ** l) -- v;;

let prostokat (x1, y1) (x2, y2) : kartka =
   fun (x, y) -> if x1 -. x <= epsilon && x -. x2 <= epsilon && y1 -. y <= epsilon && y -. y2 <= epsilon then 1
                 else 0;;

let kolko (a, b) r : kartka =
  fun (kx, ky) -> if (sqrt ((square (kx -. a)) +. (square (ky -. b)))) -. r <= epsilon then 1
                  else 0;;

let zloz p1 p2 (k : kartka) : kartka =
  fun p ->
    (* cp > 0 - p po lewej, cp < 0 - p po prawej, cp = 0 - p na prostej *)
    let cp = cross_product (p1 -- p) (p2 -- p) in
    if (abs_float cp) <= epsilon then k p
    else if cp > 0. then
      (* przesuwamy uklad o wektor -p1 i odbijamy punkt p wzgledem prostej przechodzacej przez (0,0) i (p2 -- p1) *)
      (* na koniec dodajemy z powrotem wektor p1 *)
      let refl = ((reflection (p -- p1) (p2 -- p1)) ++ p1)
      in k p + k refl
    else 0;;

let skladaj l (k : kartka) : kartka =
  List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) k l;;