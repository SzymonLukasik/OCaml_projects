(*Autor: Szymon Łukasik
Code reviewer: Jakub Jagieła *)

open PMap;;
open Array;;

(*Wyjątek podnoszony w przypadku gdy zależności są cykliczne*)
exception Cykliczne;;

(*Typ grafu: 
  size - ilość wierzchołków, 
  e - tablica zawierająca listy sąsiedztwa, 
  c - tablia zawierająca informację o kolorach wierzchołków *)
type graph = {size: int; e : int list array; c: int array};;

(*init n zwraca bezkrawędzowy graf składający się z n wierzchołków, każdy o kolorze 0*)
let init n = {size = n; e = make n []; c = make n 0};;

(*Jeśli [x]<>[y], dodaje do grafu [g] krawędź skierowaną 
łączącą wierzchołek [x] z wierzchołkiem [y].
  W przypadku gdy [x] = [y] podnosi wyjątek Cykliczne.*)
let insert_directed_edge (g:graph) (x:int) (y:int) =
  assert (x>=0 && x < g.size && y>=0 && y<g.size); 
  if x = y then raise Cykliczne;
     g.e.(x) <- y::g.e.(x);;

(*neighbours g x zwraca listę sąsiedztwa wierzchołka [x] w grafie [g]*)
let neighbours (g:graph) (x:int) =  g.e.(x);;

(* topo_sort g zwraca kolejkę posortowanych  wierzchołków w porządku odwrotnym do topologicznego
  (element na początku kolejki wynikowej jest ostatnim w porządku topologicznym)
  lub podnosi wyjątek Cykliczne w przypadku gdy graf zawiera cykl.
  Algorytm wykorzystuje przeszukiwanie w głąb.
Dla każdego wierzchołka v należącego do grafu g mamy:
  g.c.(v) = 0  gdy wierzchołek v nie jest na stosie i nie został odwiedzony
  g.c.(v) = 1  gdy wierzchołek v jest na stosie 
  g.c.(v) = 2  gdy wierzchołek v nie jest na stosie ale został już odwiedzony*)

  let topo_sort (g:graph) = 
    let q = Queue.create () and
    s = Stack.create () in 
    let dfs (v:int) = 
      if g.c.(v) = 0 then begin
        Stack.push v s; 
        let u = ref 0 in 
        while (Stack.is_empty s) = false do 
          u := Stack.top s;
          if g.c.(!u) = 0 then begin 
            g.c.(!u) <- 1;
            List.iter (fun  x -> 
              if g.c.(x) = 0 then Stack.push x s;
              if g.c.(x) = 1 then raise Cykliczne;)  g.e.(!u); 
          end
          else begin
            if g.c.(!u) = 1 then begin
              Queue.push !u q;
            g.c.(!u) <- 2;  end;
            (function x -> ()) (Stack.pop s); 
          end; done;
      end; in 
          for i = 0 to (g.size - 1) do dfs (i) done;
    q;;


(*make_map dane zwraca parę (m, i) gdzie m to mapa typu ('a * int) PMap.t 
  przypisująca elementom z listy [dane] kolejne liczby od 0 do i-1,
  gdzie i to ilość różnych elementów listy [dane]*)
let make_map (dane: ('a * 'a list) list) = 
  let i = ref 0 and m = ref empty in 
    (*add_one x dodaje (x, i) do m oraz inkrementuje i, jeśli x nie należy jeszcze do mapy. *)
  let add_one x = 
    try (function x -> ()) (find x !m) with  
      Not_found -> m := add x !i !m; i := !i + 1; in
  (*add_row (x, l) próbuje dodać do mapy element [x] oraz  elementy  listy [l] *)
  let add_row (x, l) = 
    add_one x; List.iter (fun  x -> add_one x)  l in
  List.iter (fun  row -> add_row row)  dane; (!m, !i);;

(*make_graph dane m size zwraca graf g o ilości wierzchołków równej [size], 
zbudowany na podstawie zależności z tablicy [dane] oraz 
liczbowej reprezentacji elementów z [dane] w mapie [m]*)
let make_graph (dane: ('a * 'a list) list) (m: ('a, int) PMap.t) (size: int) = 
  let  g = init size  in 
  let add_edges (x, neighbours) = 
    List.iter (fun y -> insert_directed_edge g (find x m) (find y m) ) neighbours in
  List.iter add_edges  dane; g;;

(*make_adresy [m] [element] [size] zwraca tablicę [arr] typu ('a array) i rozmiaru [size] taką, że 
  arr.(i) = a wtedy i tylko wtedy gdy para  (a, i) należy do mapy [m]. 
  (Argument [element] jest potrzebny w celu zainicjowania tablicy.) *)
let make_adresy  (m: ('a, int) PMap.t) (element: 'a) (size: int) =
  let arr = make size element in 
  foldi (fun x y _ -> arr.(y)<-x) m (); arr;; 

(*make_ans [q] [adresy] zwraca listę typu ('a list) posortowaną topologicznie 
tak, że pierszy element listy jest pierwszy w porządku topologicznym.  
[q] to kolejka typu (int Queue.t) zawierająca liczby odpowiadające poszczególnym elementom z danych wejściowych, 
posortowaną w porządku odwrotnym do topologicznego. 
[adresy] to tablica typu ('a array) zawierająca informację o tym jakiemu elementowi jest przypisana dana liczba*)
let make_ans (q: int Queue.t) (adresy: 'a array) = 
  Queue.fold (fun acc x -> adresy.(x)::acc) [] q;;

let topol (dane: ('a* 'a list) list) = 
  if dane = [] then [] else 
  let (m, size) = make_map dane in 
  let g = make_graph dane m size in 
  let q = topo_sort g and 
  adresy = make_adresy m (fst (List.hd dane)) size  in 
  make_ans q adresy;; 
  (*Niech n to wielkość danych wejściowych. 
  Wiemy, że każda operacja na mapie działa w czasie O(log(n)).
  Tworzenie mapy, grafu oraz tablicy [adresy] zajmuje O(n) pamięci i 
  przebiega w złożoności czasowej O(nlog(n)).
  Sortowanie działa w złożoności czasowej i pamięciowej O(n). 
  Zatem cały program działa w złożoności czasowej O(nlog(n)) i pamięciowej O(n). 
  *)


