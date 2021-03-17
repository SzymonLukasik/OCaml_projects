(*Autor: Szymon Łukasik
  Code reviewer: Dawid Sula 
*)
(*typ reprezentujący domknięty przedział liczb całkowitych [x, y]*)
type interval = {x: int; y: int} 
(*typ reprezentujący drzewo AVL:
  puste lub (lewe_dziecko - przedział - prawe_dziecko - wysokość - liczba_elementów_w_poddrzewie) *)
type t = Empty | Node of t * interval * t * int * int

(*Bezpieczne dodawanie - zakłada, że a i b są dodatnie - zwraca max_int lub a + b*)
let (++) (a: int) (b: int) = if a >= max_int - b then max_int else a + b 
(*Bezpieczne odejmowanie - zakłada, że a i b są dodatnie - zwraca min_int lub a - b*)
let (--) (a: int) (b: int) = if a <= min_int + b then min_int else a - b

let empty = Empty;;

let is_empty set = set = Empty;;

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(*Zwraca liczbę elementów w poddrzewie*)
let cardinality = function 
  |Node (_, _, _, _, n) -> n
  |Empty -> 0

(*Zwykły abs, tylko, że bierze pod uwage, że min_int = - max_int - 1*)
let abs a = 
  if a < 0 then 
    if a = min_int then max_int else (-a) 
  else a

(*Zwraca liczbe elementów w przedziale -
  w zależności od przypadku równą sumie 
  lub różnicy modułów jego końców powiększoną o 1*)
let card_of_interval k =  1 ++ ( 
  if (k.x > 0 && k.y > 0) then  k.y -- k.x else 
                                (*Poniżej szczególny przypadek tracenia jednego elementu w skutek przekraczania max_int*)
  if (k.x < 0 && k.y < 0) then  ( (abs k.x) -- (abs k.y)) ++ (if k.x = min_int && k.y != min_int then 1 else 0) 
  else (*k.x <=0 && k.y >=0 *)   (abs k.x) ++ k.y)

(*Zwraca sumę liczby elementów w drzewie [l], przedziale [k] i drzewie [r]*)  
let sum_card l k r = ( (cardinality l) ++ ( (cardinality r) ++ (card_of_interval k)))

(*Prawie niezmieniona funkcja z pSet - zostało dodane aktualizowanie liczebności*)
let make l k r = Node (l, k, r, max (height l) (height r) + 1, sum_card l k r )
  
(*Prawie niezmieniona funkcja z pSet*)
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
  else Node (l, k, r, max hl hr + 1, sum_card l k r)

(**Dodaje przedział [a, b] do zbioru, w którym nie zawiera się, żaden z
   elementów przedziału [a-1, b+1]*)
let rec add_one (a, b) = function
  |Node (l, k, r, h, _) ->
    if b < k.x  then 
      let nl = add_one (a, b) l in
      bal nl k r
    else (*k.y > b*)
      let nr = add_one  (a, b) r in
      bal l k nr
  |Empty -> make Empty {x=a; y=b} Empty

(*Prawie niezmieniona funkcja z pSet*)
let mem value set =
  let rec loop = function 
    |Node (l, k, r, _, _) ->
        (k.x <= value && value <= k.y) || loop (if  value < k.x then l else r)
    |Empty -> false in
  loop set

(*Prawie niezmieniona funkcja z pSet*)
let iter f set =
  let g = function k -> f (k.x, k.y) in 
  let rec loop = function
    |Empty -> ()
    |Node (l, k, r, _, _) -> loop l; g k; loop r in
  loop set

  (*Prawie niezmieniona funkcja z pSet*)
  let fold f set acc =
  let g = function k -> f (k.x, k.y) in   
  let rec loop acc = function
    |Empty -> acc
    |Node (l, k, r, _, _) ->
          loop (g k (loop acc l)) r in
  loop acc set

(*Prawie niezmieniona funkcja z pSet*)
let elements set = 
  let rec loop acc = function
    |Empty -> acc
    |Node(l, k, r, _, _) -> loop ((k.x, k.y) :: loop acc r) l in
  loop [] set

(*Prawie niezmieniona funkcja z pSet*)
let rec min_elt = function
  |Node (Empty, k, _, _, _) -> k
  |Node (l, _, _, _, _) -> min_elt l
  |Empty -> raise Not_found

(*Prawie niezmieniona funkcja z pSet*)
let rec remove_min_elt = function
  |Node (Empty, _, r, _, _) -> r
  |Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  |Empty -> invalid_arg "PSet.remove_min_elt"

(*Prawie niezmieniona funkcja z pSet*)
let merge t1 t2 =
  match t1, t2 with
  |Empty, _ -> t2
  |(_, Empty) -> t1
  |_ ->
    let k = min_elt t2 in
    bal t1 k (remove_min_elt t2)

(*Zakłada, że przedział [a, b] jest wierzchołkiem w drzewie*)
let remove_one (a, b) set =
  let rec loop = function
    |Node (l, k, r, _, _) ->
      if k.x = a then merge l r else
      if b < k.x then bal (loop l) k r else bal l k (loop r)
    |Empty -> Empty in
  loop set

(*Usuwa wszystkie elementy zawierające się w przedziale [a, b]
  dokonując przejścia po wszystkich wierzchołkach drzewa
  zawierających się w przedziale i usuwa te wierzchołki z kopii drzewa.
*)
let remove (a, b) set = 
  let rec loop acc = function
    |Node (l, k, r, _, _) ->
      if b < k.x then loop acc l else
      if a > k.y then loop acc r else
      if a <= k.x && k.y <= b then  loop (loop (remove_one (k.x , k.y) acc ) r) l else 
      if a <= k.x && b < k.y then  loop (add_one ((b ++ 1), k.y) (remove_one (k.x, k.y) acc)) l else 
      if k.x < a && k.y <= b then  loop ((add_one (k.x, ( a -- 1))) (remove_one (k.x, k.y) acc)) r else
      (*(k.x < a && b < k.y)*) add_one (( b ++ 1), k.y) (add_one (k.x, ( a -- 1)) (remove_one (k.x, k.y) acc))
    |Empty -> acc in 
  loop set set

(*Zwraca wartość typu option
  Jeśli w drzewie istnieje wierzchołek [a,b] taki, że
  [value] należy do [a,b] zwraca Optione.some {x=a; y=b}
  W przeciwnym przypadku zwraca Option.none*)
let find value set =  
  let rec loop = function
    |Empty -> Option.none
    |Node (l, k, r, _, _) ->
      if value < k.x then loop l else
      if value > k.y then loop r else
      (* k.x <= value <= k.y*) Option.some k
  in loop set

(*Dodaje przedział [a, b] do zbioru zachowując niezmienniki dotyczące: 
  niepokrywania się przedziałów, niesąsiadowaniu przedziałów
  oraz odpowiedniego wyważenia drzewa AVL. *)
let add (a, b) set =
  let l_holder = find ( a -- 1) set in 
  let r_holder = find ( b ++ 1) set in
  let lx = (if  l_holder != Option.none then (Option.get l_holder).x else a) in  
  let ry = (if r_holder != Option.none then (Option.get r_holder).y else b) in 
  add_one (lx, ry) (remove (lx, ry) set) 

(*Prawie niezmieniona funkcja z pSet*)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_one  (v.x, v.y) r
  |(_, Empty) -> add_one  (v.x, v.y) l
  |(Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join  lr v r) else
      if rh > lh + 2 then bal (join  l v rl) rv rr else
      make l v r
  
let split x set =
  let rec loop x = function
    Empty ->
      (Empty, false, Empty)
    |Node (l, v, r, _, _) -> 
      if v.x = x && x = v.y then (l, true , r) else 
      if v.x = x && x < v.y then (l, true, add_one ( x ++ 1, v.y) r) else 
      if v.x < x && x = v.y then (add_one (v.x,  x -- 1) l, true, r) else 
      if v.x < x && x < v.y then (add_one (v.x,  x -- 1) l, true, add_one ( x ++ 1, v.y) r) else 
      if x < v.x then
        let (ll, pres, rl) = loop x l in (ll, pres, join  rl v r)
      else
        let (lr, pres, rr) = loop x r in (join  l v lr, pres, rr) in
  let setl, pres, setr = loop x set in
  (setl, pres, setr)

let below x set = 
  let (setl, pres, setr) = split x set in
    (cardinality setl) ++ (if pres then 1 else 0)  
   
  
 