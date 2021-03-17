(*Autor: Szymon Łukasik
Code reviewer: Jakub Korzeniewski*)
type point = float * float

type kartka = point -> int
(*--------------POMOCNICZE--------------*)

(*Stała niedokładności przy porównaniach*)
let eps = 1e-9;;

(*wartosc bezwzględna floatów*)
let abs a = if a < 0. then (-.a) else a;; 
  
(*zwraca kwadrat podanej liczby*)
let kw x = x*.x;;

(*vect p1 p0 zwraca parę współrzędnych wektora [p0, p1] - o początku w p0 i końcu w p1*)
let vect ((x1, y1):point) ((x0, y0):point) = ((x1 -. x0, y1 -. y0):point);;

(*Zwraca 0, 1 lub -1 w zależności od znaku iloczynu wektorowego [x1, y1] x [x2, y2] *)
let cross_product ((x1, y1):point) ((x2, y2):point) = 
  let cp = (x1 *. y2) -. (y1 *. x2) in 
  if (abs cp) <= eps  then 0 else 
  if cp > eps then 1 else -1;;

(*Zwraca parę współrzędnych wektora normalnego prostej przechodzącej przez punkty (x1, y1) (x2, y2) 
  Korzystamy z faktu, że taka prosta ma postać: (y-y1)(x2-x1) = (y2 - y1)(x-x1) *)
let prosta_norm ((x1, y1):point) ((x2, y2): point) = ((y2 -. y1), (x1 -. x2));;

(*Zakłada, że b <> 0 i zwraca parę współczynników (m, n) takich, że 
  prosta o wektorze normalnym [a, b] zawierająca punkt (x, y) ma postać kierunkową
  y = mx + n                                                                    *)
let prosta_kier (a:float) (b:float) ((x, y):point) =  
  let c = (-.a)*.x +. (-.b)*.y in ((-.a)/.b, (-.c)/.b);;

(*Zwraca punkt przecięcia prostych o postaciach kierunkowych:
  y = ax + b, y = cx + d                                 *)
let przeciecie (a, b) (c, d) = 
  let x = (d-.b)/.(a-.c) in ((x, a*.x +. b):point);; 

  (*Zwraca odbicie symetryczne punktu p względem prostej przechodzącej przez punkty p1 i p2.
  Zakłada, że p leży na lewo od wektora [p1, p2]                                      *)
let odbicie ((px1, py1) as p1:point) ((px2, py2) as p2:point) ((px, py) as p :point) = 
  let (a, b) = prosta_norm p1 p2 in 
  if a = 0. then (*wektor normalny pionowy - prosta jest rownolegla do OX *) (px, py1 -.  (py -. py1)) else
  if b = 0. then (*wektor normalny poziomy - prosta rownolegla do OY *) (px1 -.  (px -. px1), py) else
  (*a <> 0 && b <> 0 *) (*Znajdujemy przecięcie podanej prostej z  prostą prostopadłą zawierającą punkt p*)
    let (px0, py0) as p0 = przeciecie (prosta_kier a b p1 ) (prosta_kier (-.b) a p) in 
    (*wyznaczamy wektor [p -> p0] i zaczepiamy go w punkcie p0 otrzymując
      współrzędne punktu przecięcia*)
    let (x, y) = vect p0 p  in ((px0 +. x, py0 +. y):point);;

(*Operator porównywania z niedokładnością*)
let (<<<) a b = a -. b <= eps;;   

(*--------------KONIEC POMOCNICZYCH------------ *)

let prostokat  ((p1x, p1y):point) ((p2x, p2y):point) = ((function ((px, py):point) ->
if  p1x <<< px && px <<< p2x && p1y <<< py && py <<< p2y 
then 1 
else 0):kartka);;

let kolko ((ox, oy):point) (r:float) = ((function ((px, py):point) ->
if ((kw (px -. ox)) +. (kw (py -. oy))) <<< (kw r) then 1 else 0):kartka);;

let zloz ((px1, py1) as p1 :point) ((px2, py2) as p2 :point) (kart:kartka) = ((function ((px, py) as p :point) ->
  let cross_p = cross_product (vect p2 p1 ) (vect p p1) in
  if cross_p = -1 then 0      (* punkt p leży na prawo od wektora [p1, p2] *)
  else if cross_p = 1 then (kart p ) + (kart (odbicie p1 p2 p))  (*punkt p leży na lewo od wektora [p1, p2] *) 
  else  kart p  (* punkt p leży na prostej przechodzącej przez punkty p1, p2*)
):kartka);;

let skladaj (lista: (point * point ) list) (kart:kartka) = 
(List.fold_left (fun (acc:kartka) (p1, p2) -> zloz p1 p2 acc) kart lista);;