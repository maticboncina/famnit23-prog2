(* produktni tipi *)
(1,2, "banana");;
type oseba={mutable ime:string;starost:int};;
let o1 ={ime="Marija Terezija";starost=80};;
match o1 with {ime=x; starost=y}->{ime=x^" 10"; starost=y+10};;

o1.ime;; (*je mutable*)
(* ni mutable --> o1.starost;; *)
o1;;

(* primeri *)
type racionalno={st:int;im:int};;
let polovica ={st=2;im=4};;
let rec nsd a b=if b=0 then a else nsd b (a mod b);;
nsd 36 60;;
let okrajsaj=function {st=x;im=y}->{st=x/(nsd x y);im=y/(nsd x y)};;
okrajsaj {st=36;im=60};;
let vsota r1 r2=okrajsaj {st=r1.st*r2.st+r1.im*r2.st;im=r1.im*r2.im};;
vsota {st=7;im=10} {st=5;im=15};;

(*unije/vsotni tipi, primer stevila*)
type stevilo = Neskoncno | Celo of int | Decimalno of float | Racionalno of racionalno | Imaginarno of float*float;;
[|Neskoncno;Celo 5;Decimalno 4.32;Racionalno {st=36;im=60};Imaginarno (3.14, 2.72)|];;

type barva = Srce | Pik | Karo | Kriz;;
type vrednost = Stevilo of int | Fant;;
type karta = {b:barva;v:vrednost};;
{b=Srce;v=Stevilo 6};;

type placilo = Preklic | Gotovina of int | Kartica of string*int;;
[|Gotovina 24;Preklic;Kartica ("Visa",10);Gotovina 13;Kartica ("Mastercard",25)|];;

let sestej_placila polje_placil=
  Array.fold_left (fun (x1,x2) y->match y with
  | Preklic ->
  | Gotovina z->
  | Kartica (_,z)->  
  ) (0,0) polje_placil;;
  
type polozaj= Kralj | Papez of string*int | Vitez of string | Kmet of string;;
let a_je_vecji_polozaj p1 p2=match (p1,p2) with 
  | (_, Kralj) -> false
  | (Kralj, _) -> true
  | (_, Papez _) -> false
  | (Papez _, _) -> true
  | (_, Vitez _) -> false
  | (Vitez _, _) -> true
  | _ -> false;;
  let(>>)=a_je_vecji_polozaj;;
  Kralj >> Papez ("Janez Pavel",2);;
  
let a_je_vecji_polozaj2 p1 p2=
  let cin=function
  | Kralj -> 4
  | Papez _ -> 3
  | Vitez _ -> 2
  | Kmet _ -> 1 in
  (cin p1)>(cin p2);;
  
a_je_vecji_polozaj2 (Kmet "Janez") (Kralj);;

type fig= Kralj | Dama | Konj | Lovec | Trdnjava | Kmet;;
type figura= Bel of fig | Crn of fig;;
Kralj;;
(Kralj:fig);;
(Kralj:polozaj);;
[Bel Kralj; Crn Kralj; Bel Dama; Bel Lovec; Crn Trdnjava; Crn Kmet];;
let moc=function
  | Kmet -> 1
  | Lovec | Konj -> 3
  | Trdnjava -> 5
  | Dama -> 9
  | Kralj -> 1000;;
moc Dama;;

let zmaguje seznam_figur=List.fold_left (fun (x1, x2) y->match y with 
  | Bel x->(x1+(moc x),x2)
  | Crn x->(x1, x2+(moc x))
  ) (0,0) seznam_figur;;
  
(*parametrizirani tipi*)
None;;
Some "AAA";;
Some 4;;
type 'a opcija=Nic | Nekaj of 'a;;

(*multi parametrizirani tipi*)
type ('a, 'b, 'c) krneki = Ananas of 'a | Banana of 'b*'c;; 