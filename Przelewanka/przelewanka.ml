(*Autor: Szymon Łukasik*)
(*Code review: Antoni Wiśniewski*)

open Array;; 

(*Wyjątek podnoszony przez procedurę [solve]
 w przypadku znalezienia wyniku*)

exception Answer of int;;

(*Procedura add_non_visited, jeśli tablica [stan]
  nie występuje w tablicy hashującej [hasht], 
  (czyli jeśli nie odwiedziliśmy jeszcze danego stanu)
  dodaje na kolejkę [queue] parę (stan, time) 
  oraz dodaje do tablicy hashującej
  [hasht] wiązanie (stan, 0). 
  To, że  wiążemy tablicę [stan] akurat z liczbą 0 
  nie ma znaczenia. Dla algorytmu istotna jest tylko 
  informacja  czy dana tablica znajduje się w tablicy 
  hashującej czy nie.*)
let add_non_visited queue hasht (stan: int array) (time: int) =
  if not (Hashtbl.mem hasht  stan) then begin
    Queue.push  (copy stan, time) queue;
    Hashtbl.add hasht (copy stan) 0; 
  end;;

(*Procedura przyjmująca jako argumenty 
tablicę objętości szklanek
i tablicę ich oczekiwanego napełnienia. 
Wywokonuje przeszukiwanie BFS w grafie stanów 
i w przypadku gdy znajdzie optymalne rozwiązanie
zwraca liczbę potrzebnych czynności. W przeciwnym
przypadku zwraca -1. *)
let solve (volume: int array) (expected: int array) = 
  let n = length volume in 
  let tab = ref ( make n 0) and 
  perm = ref (make n 0) and
  ans = ref 0 and

  (*Kolejka służąca do przeszukiwania grafu stanów. 
  Będzie zawierać elementy typu ((int array ) * int) 
  reprezentujące obecne napełnienie szklanek i 
  liczbę czynności potrzebną do jego uzyskania.*)
  bfs = Queue.create () and

  (*Tablica hashująca przetrzymująca 
  informację o tym jaki stan już został odwiedzony.*)
  hasht = Hashtbl.create 0  in 
  try 
    (*Dodajemy do kolejki i tablicy hashów pierwszy
      stan - wszystkie szklanki są w nim puste*)
    add_non_visited bfs hasht !tab 0;

    (*Jeśli oczekiwane napełnienie wszystkich szklanek
      wynosi zero - zwracamy wynik...*)
    if Hashtbl.mem hasht expected then raise (Answer 0);

    (*...w przeciwnym wypadku
      rozpoczynamy przeszukiwanie w szerz grafu stanów.*)
    while not (Queue.is_empty bfs) do 
    
      (*aktualne napełnienie szklanek*)
      tab := fst (Queue.peek bfs); 

      (*liczba czynności potrzebna do jego osiągnięcia*)
      ans:= snd (Queue.take bfs);  
      ans := !ans+1;

      (*Tablica [perm] będzie służyć do dodawania kolejnych stanów na kolejkę*)
      perm := copy !tab;

      (*Rozważamy czynności związane z i-tą szklanką*)
      for i = 0 to n-1 do 

        (*Jeśli i-ta szklanka nie jest pusta, wylewamy z niej wodę*)
        if !tab.(i)<>0 then begin
          !perm.(i) <-0;
          add_non_visited bfs hasht !perm !ans;
          if Hashtbl.mem hasht expected then raise (Answer !ans);
          !perm.(i) <- !tab.(i); 
        end;

        (*Jeśli i-ta szklanka nie jest pełna, napełniamy ją wodą*)
        if !tab.(i) <> volume.(i) then begin 
          !perm.(i) <- volume.(i);
          add_non_visited bfs hasht !perm !ans ;
          if Hashtbl.mem hasht expected then raise (Answer !ans);
          !perm.(i) <- !tab.(i); 
        end;

        (*Przelewamy wodę z i-tej szklanki do innych*)
        for j=0 to n-1 do 
          if i<>j then begin
            !perm.(j) <- min (!tab.(j) + !tab.(i)) volume.(j);
            !perm.(i) <- !tab.(i) - (!perm.(j) - !tab.(j));
            add_non_visited bfs hasht !perm !ans;
            if Hashtbl.mem hasht expected then raise (Answer !ans);
            !perm.(i) <- !tab.(i);
            !perm.(j) <- !tab.(j);
          end;
        done;
      done;
    done; -1
  with (Answer ans)-> ans;;

(*Procedura rekurencyjna zwracająca nwd liczb [a] i [b]*)
let rec nwd a b = 
  if b = 0 then a 
  else nwd b (a mod b ) ;;
  
(*Procedura zwracająca [false] w przypadku gdy jeden z warunków 
  koniecznych do uzyskania wyniku są niespełnione i 
  [true] w przeciwnym przypadku.*)
let warunek_konieczny (volume: int array) (expected: int array) = 
  (*obliczamy nwd objętości wszystkich szklanek*)
  let g = fold_left nwd 0 volume in (if g <> 0 then
  (*Jeśli g<>0 sprawdzamy czy oczekiwane napełnienie każdej
    szklanki jest podzielne przez nwd wszystkich objętości.*)
  fold_left (fun acc x -> acc = (x mod g = 0)) true expected else true) 
  (*Sprawdzamy drugi warunek czyli czy istnieje szklanka, 
    której oczekiwane napełnienie wynosi zero 
    lub jest równe jej objętości*)
  && not (for_all2 (fun x y -> x <> y && y <> 0) volume expected);; 

(*Procedura przelewanka przyjmuje jako argument tablicę 
  par opisaną  w treści zadania i zwraca liczbę
  czynności potrzebną do uzyskania oczekiwanego napełnienia
  szklanek, lub jeśli nie jest to możliwe - zwraca -1. *)
let przelewanka  (dane: (int * int) array) = match dane with
  [||] -> 0 (*Rozważamy skrajny przypadek pustej tablicy*)
  |_-> (*Tworzymy dwie osobne tablice reprezentujące
        odpwoiednio objętości i oczekiwane napełnienie szklanek.*)
    let volume = (map fst dane) and 
    expected = (map snd dane) in 
    (*Jeśli spełniony jest warunek konieczny, wywołujemy procedurę solve
      w przeciwnym wypadku zwracamy -1*)
    if warunek_konieczny volume expected 
      then solve volume expected
      else -1;; 

(*Algorytm działa w złożoności pamięciowej i czasowej
  O(n*m) gdzie n to długość tablicy wejściowej [dane],
  a m to liczba wierzchołków w grafie stanów.*)
    
