(*_____________________________ AUTOR: SZYMON ŁUKASIK__________________________________
 *__________________________ CODE REVIEWER: JAKUB PODOLAK______________________________*)                          

(*_____________________________________IMPLEMENTACJA___________________________________*)

            type wartosc = Inte of float * float | Comple of float * float | Full | Nan;;
            (*                          Opis wariantow: 
             * Inte - przedzial [l, r] gdzie:
			      l<=r oraz l,r naleza do rzeczywistych (gdy l=r mamy zbiór jednoelementowy)
			      lub l = neg_infinity i r jest rzeczywiste (reprezentuje przedział nieograniczony od dołu)
			      lub r = infinity i l jest rzeczywiste		(reprezentuje przedział nieograniczony od góry)
             * Comple - dopelnienie przedzialu czyli suma przedzialow:
             * (neg_infinity, l] U [r, infinity) gdzie l < r oraz l,r naleza do rzeczywistych
             * Full - przedzial (neg_infinity, infinity)
             * Nan - przedzial pusty 
             *)

 (*_____________________________Procedury ogólne*)
          
            let failwith s = raise (Failure s);;
            let classf a = classify_float a;;
            let min (a:float) (b:float) = if a<b then a else b;;
            let max (a:float) (b:float) = if a>b then a else b;;
            let abs (a:float) = if a < 0. then (-.a) else a;; 
            
            (*procedura make_full dba o to by w przypadku uzyskania w wyniku jakiejs operacji
             * przedzialu (neg_infinity, infinity) nie reprezentowac go jako typ Inte ani Comple, 
             * tylko zamienia wynik na typ Full.
            Jest aplikowana przed zwroceniem wyniku przez procedury "plus" oraz "razy"
            za pomocą, których są zaimplementowane operacje dodawania, odejmowania, mnożenia i dzielenia
            przedziałów.*)

            let make_full zbior = match zbior with 
            |Comple(l, r) ->
              if l >= r then Full else Comple(l, r)    
            |Inte (l, r) -> 
              if l=neg_infinity && r=infinity then Full else Inte(l, r)
            |Full -> Full
            |Nan -> Nan;;
           
            
            (*procedura przeciwny zwraca przedzial powstaly poprzez pomnozenie przez
             * -1 kazdego z elementow przedzialu*)

            let przeciwny zbior = match zbior with
            |Inte (l, r) -> Inte (-.r, -.l)    
            |Comple (l, r) -> Comple (-.r, -.l)
            |Full -> Full
            |Nan -> Nan;;

  (*______________________________konstruktory*)
            
            let wartosc_dokladnosc x p = Inte ((x -. abs(x*.(p/.100.0))), (x +. abs(x*.(p/.100.0))));;
            let wartosc_od_do x y = Inte (x, y);;
            let wartosc_dokladna x = Inte (x, x);;
            
   (*_____________________________*selektory*)

            let in_wartosc zbior punkt = match zbior with
            |Inte (l, r) -> if (l<=punkt && punkt<=r) then true else false
            |Comple (l, r) -> if (punkt<=l || punkt>=r) then true else false
            |Full -> true
            |Nan -> false;;

            let min_wartosc zbior = match zbior with
            |Inte (l, _) -> l
            |Comple (_, _)-> neg_infinity
            |Full -> neg_infinity
            |Nan -> nan;;

            let max_wartosc zbior = match zbior with
            |Inte (_,r) -> r
            |Comple (_, _) -> infinity
            |Full -> infinity
            |Nan -> nan;;
          
            let sr_wartosc zbior = match zbior with
            |Inte (l, r) ->(l +. r)/. 2.0  
            |Comple (_, _) -> nan
            |Full -> nan
            |Nan -> nan;;

  (*______________________________modyfikatory*)

                                  (*dodawanie i odejmowanie*) 
            (*Procedura plus zwraca zbiór {a+b | a in zbior1 && b in zbior2*)
            let plus zbior1 zbior2 = make_full (match (zbior1, zbior2) with 
            |(Nan, _) -> Nan
            |(_, Nan) -> Nan
            |(Inte (l1, r1), Inte (l2, r2) ) -> Inte (l1 +. l2, r1 +. r2)
            |(Inte (l1, r1), Comple (l2, r2) ) -> Comple (r1 +. l2, l1 +. r2)
            |(Comple (l1, r1), Inte (l2, r2) ) -> Comple (l1 +. r2, r1 +. l2)
            (*Powyższe dwa przypisania działają również w przypadku gdy jeden z końców przedziału jest infinity lub
             neg_infinity, ponieważ wynik jest przetwarzany przez procedurę make_full i zwracany jest przedział pełny Full*)                                       
            |(Comple (_, _) , Comple (_, _) ) -> Full
            |(Full, _) -> Full
            |(_, Full) -> Full );;  
            
            (*Procedura minus zwraca zbiór {a-b | a in zbior1 && b in zbior2*)
            let minus zbior1 zbior2 = plus zbior1 (przeciwny zbior2);;
            (*przy odejmowaniu dodajemy zbior1 do zbior2 pomnozonego przez (-1)*)



                        (*procedury pomocnicze wykorzystywane w implementacji*)
                                (*operacji mnożenia oraz dzielenia*)
           

            (*Procedura odwrotność zwraca zbiór "wynik" := {1/a | a in zbior && a!=0.0}
            czyli zbiór wartości funkcji f(x)=1/x dla x in zbior i x!=0.0.
            Operując na zbiorach nieskończonych korzystamy z faktu, że 
            dla x -> infinity 1/x -> 0 oraz dla x -> neg_infinity 1/x -> 0,
            oraz założenia, że zbior "wynik" może zawierać zero, co jest 
            zgodne z treścią polecenia, która mówi o domykaniu zbiorów
            przez modyfikatory.*)
            let odwrotnosc zbior = make_full ( match zbior with
            (* Uwaga dotycząca odwrotności przedziałów: 
             * Przedziały mogą być również nieskończone to znaczy Inte(l, r)
             * moze byc taki, ze l = neg_infinity i r rzeczywiste lub l rzeczywiste i r=neg_infinity
             * Analiza tych przypadków nie jest skomplikowana, a poniższa implementacja je rozważa
             * wykorzystująć własność typu float - 1/.infinity = 0.0 oraz 1/.neg_infinity = (-0.0)
             * Jednak nie bedzie to powodowalo problemow przy dalszym korzystaniu z wynikowego przedzialu,
             * a w szczególności przy porównywaniu jego końców z 0.0 w niektórych procedurach
             * poniewaz specyfikacja typu float mówi, że (-0.0) = (0.0)*)   

                                                                          (*Obrazowe przedstawienie wyniku operacji odwrotnosc*)
            |Inte (l, r) ->                                                (*W przypadku przedziałów, tylko przykłady skończonych*)
                if l > 0. || r < 0.  then Inte (1. /. r, 1. /. l) else      (* np.    O  (---)  ===>    (---O--)            *)
                if l = 0. && r = 0. then Nan else                           (*      (O)         ===>    zbiór pusty         *)
                if l < 0. && r > 0. then Comple (1. /. l, 1. /. r) else     (*     (--O--)      ===>    -----)  O  (-----   *)
                if l = 0. && r > 0. then Inte (1. /. r, infinity) else      (*  (O-----)        ===>      O  (--------      *)
                (* l < 0. && r = 0. *) Inte (neg_infinity, 1. /. l)         (*  (-----O)        ===>     --------)   O      *)    
            |Comple (l, r) ->
              if l > 0. || r < 0. then Comple (1. /. r, 1. /. l) else       (*np. -----) (-----O--- ===>   -----)  (----O-----*) 
                if l = 0. && r > 0. then Inte (neg_infinity, 1. /. r) else  (*----------O) (-----   ===>    -----------O--)   *)                  
                if l < 0. && r = 0. then Inte (1. /. l, infinity) else      (* --------)  (O------- ===>    (---O-------      *)  
                (*if l < 0. && r > 0.*)  Inte (1. /. l, 1. /. r)            (* ------) O  (-------- ===>    (---O---)         *) 
            |Full -> Full
            |Nan -> Nan );;  



            (*posinte_x_comple - mnozenie przedzialu o DODATNICH wartosciach przez dowolne dopełnienie*)
            let posinte_x_comple inte comple = match (inte, comple) with
            |(Inte(l1, r1), Comple(l2, r2)) ->                            (*Obrazowe przedstawienie postaci dopełnień oraz
                                                                                    komentarz dotyczący intuicji.*)

                if l2 > 0. then  (Comple(l2 *. r1, r2 *. l1))             (*     ------O----)  (--------------          *)   
                                                                        (*lewy koniec chcę oddalić, a prawy zbliżyć do zera*)

                else if r2 < 0. then  (Comple (l2 *. l1, r2 *. r1))       (*             ---------) (---O-------        *)
                                                                         (*lewy koniec chcę zbliżyć do zera, a prawy oddalić*)

                else (*l2 <= 0 && r2 >= 0 *) (Comple(l2 *. l1, r2 *. l1)) (*np. ------O) (------- lub -----) O (-----   *)
                                                                        (*zarówno lewy jak i prawy koniec chcę zbliżyć do zera*)
            |(_, _) -> failwith "Zle argumenty posinte_x_comple";;



            (*inte_x_comple - mnozenie dowolnego przedzialu przez dowolne dopelnienie*)
            let inte_x_comple inte comple = match (inte, comple) with
            |(Inte(l1, r1), Comple(l2, r2)) -> 
                if (l1 = 0. && r1 = 0.) then Inte (0., 0.)  (*przedzial jest zerem*)

                else if in_wartosc (Inte(l1, r1)) 0.0 then Full (*przedzial jest otoczeniem zera*)
                  (*Zwracamy Full ponieważ każdą liczbę x możemy wyrazić jako iloczyn dowolnie małej liczby x/y
                   (ktora w naszym przypadku nalezy do otoczenia zera) i dowolnie duzej liczby y ktora
                   nalezy do dopelnienia.*)

                else if r1 < 0. then przeciwny (posinte_x_comple (przeciwny (Inte(l1, r1))) (Comple(l2, r2)))
                    (*korzystamy z własności (-1)*(-1)=1, łączności oraz przemienności mnożenia
                     *mnożąc przedział przez (-1), obliczając wynik dla iloczynu 
                      dodatniego przedzialu i dopełnienia, a następnie mnożąc ten wynik przez (-1)*)  

                else posinte_x_comple (Inte(l1, r1)) (Comple(l2, r2))
                    (*przedzial juz ma dodatnie wartosci - wywolujemy posinte_x_comple *)

            |(_, _) -> failwith "Zle argumenty inte_x_comple";;
            


            (*AUX - mnozenie dowolnego dopelnieniea przez dowolne dopelnienie*)
            (*zauwazamy, ze tylko iloczyny dopelnien otoczeń zera daja w wyniku inny przedzial 
              niz (neg_infinity, infinity) i kiedy  taki mamy wystarczy wziac maksymalna
              z ujemnych iloczynow elementow i  minimalna z dodatnich iloczynow elementow. 
              Zobrazowanie tego przykladu:
                 -----) O (------    razy    -----) O (------                  *)
            let comple_x_comple comple1 comple2 = match (comple1, comple2) with
            |(Comple(l1, r1), Comple(l2, r2)) -> 
                if (l1 <= 0. && r1 >= 0.) && (l2 <= 0. && r2 >= 0.) 
                then Comple (max (l1*.r2) (r1*.l2), min (l1*.l2) (r1*.r2))
                else Full 
            |(_, _) -> failwith "Zle argumenty comple_x_comple";;
         
                                  (*mnozenie i dzielenie*)   
            (*Procedura razy zwraca zbiór {a*b | a in zbior1 && b in zbior2*)
            let razy zbior1 zbior2 = make_full (match (zbior1, zbior2) with 
            |(Nan, _) -> Nan
            |(_, Nan) -> Nan  
            |(Inte (l1, r1), Inte (l2, r2) ) 
             (*W tym przypadku obliczamy wszystkie 4 iloczyny i bierzemy z nich
               minimum oraz maksimum, ale musimy uwazac na mnozenie przedzialu 
               nieskończonego np. (3, infinity) przez (0, 4) poniewaz 
               wg. specyfikacji typu float mamy, że (neg_)infinity *. 0. = nan itd.
               W konsekwencji moglibyśmy przypisać kresowi wynikowego przedziału
               wartość nan, co nie jest przewidziane w definicji typu Inte.
               Rozwiazujemy problem zamieniajac nan na 0. A nie sprawia to problemów 
               ponieważ jeśli otrzymaliśmy nan w wyniku mnożenia to tylko w wyniku  
               mnożenia nieskończoności przez zero, a przecież nasze przedziały są domknięte
               do zera i możemy osiągnąć w zbiorze wynikowym wartość 0. *)
                -> (let (a, b, c, d) = (l1*.l2, l1*.r2, r1*.l2, r1*.r2) in
                        let a = if classf a = FP_nan then 0. else a in
                        let b = if classf b = FP_nan then 0. else b in
                        let c = if classf c = FP_nan then 0. else c in
                        let d = if classf d = FP_nan then 0. else d in
                   let minimum = min a (min b (min c d))
                   and maksimum = max a (max b (max c d))    
                in Inte (minimum, maksimum))
            |(Inte (l1, r1), Comple (l2, r2)) -> inte_x_comple (Inte (l1, r1)) (Comple(l2, r2))                   
	          |(Comple (l1, r1), Inte (l2, r2)) -> inte_x_comple (Inte(l2, r2)) (Comple(l1, r1))
            (*^ tutaj zamieniamy kolejnosc argumentów korzystając z przemienności mnożenia*) 
            |(Comple (l1, r1), Comple (l2, r2)) -> comple_x_comple (Comple(l1, r1)) (Comple(l2, r2))
            |(_, Inte(0., 0.)) -> Inte (0., 0.) 
            |(Inte(0., 0.), _) -> Inte (0., 0.)
            (*cokolwiek przez zero to zero*)
            |(Full, _) -> Full
            |(_, Full) -> Full);;
            
            (*Procedura podzielic zwraca zbiór {a/b | a in zbior1 && b in zbior2 && b!=0*)
            let podzielic zbior1 zbior2 = razy zbior1 (odwrotnosc zbior2);;
             (*przy dzieleniu mnozymy zbior1 przez odwrotnosc zbior2*)



