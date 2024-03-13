(*VAJE3*)

(*rekurzija*)
(fun x->x+1) 4;;
let f x=x+1;;
f 10;;
if true then 1 else 0;;

let rec sestej_do_n n=
  if (n<=1) then 1 else n+(sestej_do_n (n-1));;
sestej_do_n 100;;  

[1;2]@[3;4];;
0::[1;2;3;4];;
List.hd [1;2;3;4];;
List.tl [1;2;3;4];;

let rec zdruzi_seznama sez1 sez2=
  if(sez1=[]) then sez2
  else zdruzi_seznama (List.tl sez1) ((List.hd sez1)::sez2);;
  
zdruzi_seznama [1;2;3] [4;5];;
zdruzi_seznama [2;3] [1;4;5];;
zdruzi_seznama [3] [2;1;4;5];;
zdruzi_seznama [] [3;2;1;4;5];;

let rec zdruzi_seznama_pravilno sez1 sez2=
  if(sez1=[]) then sez2
  else (List.hd sez1)::(zdruzi_seznama_pravilno (List.tl sez1) sez2);;

zdruzi_seznama_pravilno [1;2;3] [4;5];;
1::zdruzi_seznama_pravilno [2;3] [4;5];;
1::2::(zdruzi_seznama_pravilno [3] [4;5]);;
1::2::3::(zdruzi_seznama_pravilno [] [4;5]);;
1::2::3::[4;5];;


let a=1 in a+2;;
let b=let a=1 in a+2;;
b;;

let obrni_seznam seznam=
  let rec zdruzi_seznama sez1 sez2=
    if(sez1=[]) then sez2
      else zdruzi_seznama (List.tl sez1) ((List.hd sez1)::sez2) in
  zdruzi_seznama seznam [];;
  
obrni_seznam [1;2;3;4;5;6;7];;

fst (1,2);;

(*ujemanje vzorcev?*)
match (1,2,"banana",'c') with (a,b,c,d)->c;;

let tretji stiris=match stiris with (a,b,c,d)->c;;
tretji (1,'s',4,2.9);;

let tretji =function (a,b,c,d)->c;;
tretji (1,'s',4,2.9);;

let tretji (a,b,c,d)=c;;


(&&) true true;;
(&&) true false;;
(&&) false true;;
(&&) false false;;


let implikacija p q=match (p,q) with
  | (true,true) -> true
  | (true,false) -> false
  | (false,true) -> true
  | (false,false) -> true;;
  
implikacija true false;;

let implikacija p q=match (p,q) with
  | (true,true)| (true,false) | (false,true) | (false,false) -> true
  | (true,false) -> false;;
  
let implikacija p q=match (p,q) with
  | (_,true) | (false,false) -> true
  | (true,false) -> false;;
  
let implikacija p q=match (p,q) with
  | (true,false) -> false
  | _ -> true;;

true && false;;
(&&);;
(* (=>);; ne obstaja trenutno*)

let (=>) p q=match (p,q) with
  | (true,false) -> false
  | _ -> true;;

(=>) true true;;
true => false;;

(* napisi funkcijo za dolzino seznama *)
let rec dolzina_seznama seznam= match seznam with
  | [] -> 0
  | h::t -> 1+(dolzina_seznama t);;
  
dolzina_seznama [1;2;3;5;6;3;2;0;-1];;


(*napisi funkcijo ki preveri ali je element v seznamu*)

(*let je_element elm seznam=match seznam with *)
let rec je_element elm=function
  | [] -> false
  | h::_ when h=elm -> true
  | _::t -> je_element elm t;;

je_element 3 [1;23;4;3;5];;
je_element 0 [1;23;4;3;5];;

je_element "a" ["banana";"Koper"; "Na zdravje!"];;
je_element "Koper" ["banana";"Koper"; "Na zdravje!"];;


(*ne dela na seznamih *)
min;;
min 4.5 7.;;
(*napisi minimum za sezname*)
let rec minimum_za_seznam=function
  (*|[] -> raise Invalid_input*)
  (*|[] ->*)
  |h::[] -> h(*[h]*)
  |h1::h2::t when h1>h2-> minimum_za_seznam (h2::t)
  |h1::h2::t -> minimum_za_seznam (h1::t);;
minimum_za_seznam [1;2;3;4;5;6;3;2;0;-1;2;3;2];;

let rec minimum_za_seznam2=function
  |h::[] -> h(*[h]*)
  |h1::h2::t -> minimum_za_seznam2 ((min h1 h2)::t);;
minimum_za_seznam2 [1;2;3;4;5;6;3;2;0;-1;2;3;2];;


(*napisi funkcijo explode ki dani niz spremeni v char list*)
"Projektor";;
"Projektor".[0];;
"Projektor".[1];;
String.length "Projektor";;

let explode beseda=
  let rec pomoznaFakeForZanka seznam i=match i with 
    | -1 -> seznam
    | x -> pomoznaFakeForZanka ((beseda.[x])::seznam) (i-1) in
  pomoznaFakeForZanka [] ((String.length beseda)-1);;
  
explode "Projektor";;
explode "";;






