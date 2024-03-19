(* funkcija ki spreminja lower v upper in obratno*)
let swap_upper_lower crka=match crka with 
| 'a'..'z' -> char_of_int ((int_of_char crka)-32)
| 'A'..'Z' -> char_of_int ((int_of_char crka)+32)
| _-> crka;;

(* iterator *)
let iterate f elm n=
let rec pomoznafakeforzanka elm n=match n with
| 0 -> elm
| x -> pomoznafakeforzanka (f elm) (x-1) in
if n<=0 then elm else pomoznafakeforzanka elm n;;

(* definiraj potenco ya integerje *)
let (^-^) osnova potenca=iterate (( * )osnova) 1 potenca;;

(* funkcija ki primerjva dva seznama *)
let rec enako_dolga sez1 sez2=match (sez1,sez2) with 
| ([],[]) -> true
| ([],_) | (_,[]) -> false
| (_::t1,_::t2) -> enako_dolga t1 t2;;

(* vstavi - mesto = crka, stevilka = vrednost *)
let rec vstavi_v_l2 (kljuc, vrednost)=function
  | [] ->(kljuc, vrednost)::[]
  | (hkljuc,hvrednost)::t when kljuc<hkljuc -> (kljuc,vrednost)::(hkljuc,hvrednost)::t
  | (hkljuc,hvrednost)::t when kljuc=hkljuc -> (hkljuc,hvrednost+vrednost)::t
  | (hkljuc,hvrednost)::t -> (hkljuc, hvrednost)::(vstavi_v_l2 (kljuc,vrednost) t);;    
  
List.map;;
List.map ((+)1) [1;2;3;4];;

List.filter;;

(* vrni samo poyitivna stevila *)
List.filter (fun x->x>0) [-3;2;3;6;-4;-9;21;0;-5];;

(* vrni samo upper case *)
List.filter (function | 'A'..'Z'->true | _ -> false)['a';'B';'c';'d';'E'];;

(* vrni true če ima črke stevilke in presledek*)
let explode str=
let rec pomozna lst=function
| 0 -> lst
| x -> pomozna ((str.[x-1])::lst) (x-1) in
pomozna [] (String.length str);;
explode "Vecer";;
let je_geslo niz=
List.for_all (function | 'A'..'Z' | 'a'..'z' | '0'..'9' | ' '-> true | _->false)(explode niz);;

(*vsota seznama*)
List.fold_left (fun x y->x+y) 0 [-3;2;-4;3;2;0;1];;

(*dolzina seznama*)
List.fold_right (fun x y->y+1) [-3;2;-4] 0;;
(*obrni seznam*)
List.fold_left (fun x y ->y::x) [-3;2;-4;3;2;0;1];;
(*max iy seznama*)
let max_seznam=function h::t->List.fold_left (fun x y -> if x>y then x else y) h t;;
max_seznam [-3;2;3;6;-4;-9;21;0;-5];;
