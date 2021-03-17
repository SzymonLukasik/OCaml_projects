(* Autor: Szymon Łukasik
   Code reviewer: Sylwia Szunejko*
*)
(*
                Definicja typu:
 * Null - wierzchołek pusty
 * Node - wierzchołek reprezentowany przez krotkę: 
    (lewe dziecko, (priorytet, npl), prawe dziecko) gdzie spełnione są warunki:

   -priorytet wierzchołka jest mniejszy bądź równy priorytetowi każdego z jego dzieci [A1]

   -npl lewego dziecka >= npl prawego dziecka gdzie:                                  [A2]
    npl (null path length) to ilość krawędzi leżących na najkrótszej ścieżce od danego 
    wierzchołka do wierzchołka pustego leżącego w jego poddrzewie.
    Zatem przyjmujemy, że dla wierzchołków będących liśćmi lub wierzchołków mających 
    tylko jedno niepuste dziecko wartość npl jest równa 1.
    Przyjmujemy, że wierzchołki puste mają wartość npl równą 0. 
*)
type 'a queue = Null | Node of 'a queue * ('a * int ) * 'a queue;;

(*Wyjątek podnoszony w przypadku gdy następuje próba usunięcia elementu z pustej kolejki*)
exception Empty;;

let failwith s = raise (Failure s);;

(*_____________________Konstruktory *)
(*Konstruktor tworzący pustą kolejkę*)
let empty = Null;;

(*_____________________Modyfikatory *)

            (*funkcja rekurencyjna łącząca ze sobą dwie kolejki*)
(*Jako argumenty przyjmuje dwie kolejki, które spełniają warunki [A1, A2] i zwraca kolejkę spełniającą warunki [A1, A2]*)
let rec join (q1: 'a queue) (q2: 'a queue) = match (q1, q2) with
|(Null, _) -> q2
|(_, Null) -> q1   (*Wynikiem złaczenia dowolnej kolejki "q" z kolejką pustą jest kolejka "q"*)

|(Node(l1, (p1, npl1), r1), Node(l2, (p2, npl2), r2)) ->
  
  if(p1>p2) then join q2 q1 
    (*wywołujemy się z zamienioną kolejnością argumentów*)
  
  else (* teraz wiemy, że p1<=p2 więc korzeń q1 będzie korzeniem nowego 
	kopca aby spełnione było [A1] *)
    
    let nowy = join r1 q2 in 
        (*łączymy prawe poddrzewo q1 z całym  q2. 
          Uwaga - wiemy, że q2 jest niepuste więc nowy tez jeset niepusty.*)
    
    if l1 = Null then Node(nowy, (p1, 1), Null) 
       (*jesli l1 jest pusty (npl = 0) i wiemy, że "nowy" jest niepusty (npl >=1)
         to aby zawsze spełniony był warunek [A2], "nowy" powinien być lewym 
        poddrzewem w wyniku. Zwracane drzewo ma npl równe 1 ponieważ 
	jego korzeń ma puste dziecko.*)
    else
    	match (l1, nowy) with 
      	|(Node(_, (_, npl_lewego), _), Node(_, ( _, npl_nowego), _)) ->
        	if npl_lewego < npl_nowego then Node(nowy, (p1, npl_lewego + 1), l1)
        	else Node(l1, (p1, npl_nowego + 1), nowy)      
     (*W przypadku gdy l1 jest niepusty, porównujemy wartosci npl "nowy" z npl "l1" 
      i zwracamy drzewo spełniające [A2], przypisując korzeniowi
      nową wartość npl równą wartości npl prawego poddrzewa powiększoną o 1 *)
      |_, _ -> failwith "Nieoczekiwane";;
      (*Nigdy nie podniesiemy powyższego wyjątku ponieważ zarówno l1 jak i nowy
        są niepuste. Chcemy jednak uniknąć zbędnego warningu kompilatora.*)
;;

(*Procedura "add" dodaje do kolejki nowy element łącząc starą kolejkę z jednoelementową kolejką 
zawierającą nowy element (nowa kolejka ma npl równe 1 ponieważ jest liściem)*)
let add (nowy: 'a) (kolejka: 'a queue) = join  (Node(Null,(nowy, 1), Null))  kolejka;;

(*Procedura "delete_min" usuwa z kolejki korzeń (element o najmniejszym priorytecie) 
i zwraca parę złożoną z usuniętego elementu i powstałej po usunięciu kolejki.
  W przypadku gdu "kolejka" jest pusta podnosi wyjątek "Empty"*)
let delete_min (kolejka: 'a queue) = match kolejka with 
|Null -> raise Empty
|Node(left, (p, _), right) -> (p, (join left right));;

(*____________________Selektory *)

(*Procedura "is_empty" zwraca wartośc logiczną 
 - "true" jeżeli "kolejka" jest pusta
 - "false" w przeciwnym wypadku.*)
let is_empty (kolejka: 'a queue) = match kolejka with
|Null -> true
|_ -> false;;


                                                         

      
