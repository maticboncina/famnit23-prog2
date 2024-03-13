(fun x->x+1) 4;;

predhodnik 2;;

let a=1 in a+2;;
let b=let a=1 in a+2;;

let identiteta x=
let predhodnik x=x-1 in
let naslednik x=x+1 in
predhodnik (naslednik x);;
identiteta 10;;

[1;2;4;6];;
List.hd [1;2;4;6];;
List.tl [1;2;4;6];;
[];;

if true then 3 else 4;;

0::[1;2;3];;
(*:: dodaja v glavo*)

[1;2]@[3;4];;(*Nesveš nuct znotraj rekurzije*)

(*spodi dodajamo v sezname*)
let rec append_inverse sez1 sez2 =
  if(sez1=[]) then sez2 else
    append_inverse(List.tl sez1) ((List.hd sez1)::sez2);;

    append_inverse [1;2;3] [4;5];;

let rec append sez1 sez2=
    if(sez1 = []) then sez2 else
      (List.hd sez1)::(append (List.tl sez1)sez2)(*To sranje levo krajša seznam*);;
append [1;2;3] [4;5];;

let rec kontra seznam=
let rec append_inverse sez1 sez2 =
  if(sez1=[]) then sez2 else
    append_inverse(List.tl sez1) ((List.hd sez1)::sez2) in
    append_inverse seznam [];;

kontra [1;2;3;6;123];;

let rec od_n_do_ena n=
    if(n <= 1) then [1] else
      n:: (od_n_do_ena (n-1));;

od_n_do_ena 10;;

match (1,2,"tri") with (a,b,c)->c;;

match[1;3] with []->0 | h::t->1;;
match[1;3] with []->0 | _->1;; (*Podčrtaj je karkoli _*)

let tretji x=match x with (a,b,c)->c;;
let tretji2 =function (a,b,c)->c;;(*če iščeš po zadnjem parametru lahko nadomestis*)
let tretji3 (a,b,c)=c;;

tretji (1,2,3);;
tretji2 (1,2,"tri");;
tretji3(3,"wasu",3);;


(*Funkcija za implikacijo*)
(*una implikacija*)
(*(&&) true false;;*)
let implikacija levo desno= match (levo,desno) with
  | (true,true)->true
  | (true,false)-> false
  | (false,true)->true
  | (false,false)->true;;
  (*stupid way of doing it*)

  (*zdej bo malo manj neumen*)
  let iwasu levo desno= match (levo,desno) with
  | (true,true) | (false,true)| (false,false)->true
  | (true,false)-> false;;
(*Lahko matches na vč različnih načinov*)
  let ipupščica levo desno= match (levo,desno) with
  | (_,true) |(false,false)->true;;
  | (true,false)-> false;;

  (*Smart way of doing it*)
  let implikacijaSmart levo desno = match (levo,desno) with 
  | (true,false)->false
  | _ -> true;;

(*Implikacija smartt ++ engineer level design*)
  let (=>) levo desno = match (levo,desno) with (*Tku daš ime => funkciji*)
  | (true,false)->false
  | _ -> true;;

    true => false;;
    (=>) false true;;
      (*te zadnje dve vrstice delajo samo z znaki*)
      
      (*Pazi de ne povoziš + -  kr čene si lahka fpizdi*)
      (*let (+) a b = a-b;;
      (+) 3 2;; če ne vrjameš runej to codo *)

(* dolzina seznama*)
(*function je obvezen za pattern matching*)
(* lahka tudi let rec dolzina_seznama sez = match sez with *)
let rec dolzina_seznama= function
| [] -> 0
| h::t->1+(dolzina_seznama t);;
dolzina_seznama [1;2;3;6;5]

(*let je_elkement elt sez = match sez with*)

(*De pogledaš če je elt znotraj*)
let rec je_element elt=function
| []->false
| h::t when h=elt->true
| h::t->je_element elt t;;
je_element 1 [2;3;4;5;1;2];;
je_element 121 [2;3;1;2;3;5;3];;
 (*Fibonacci supreme ultra high hd*)
 (*če maš sam 1 parameter nrdiš patern matching*)
let rec fibonacci = function
|1 -> [1]
|2 -> [1;1]
|n -> let prejsnji=fibonacci(n-1) in
((List.hd prejsnji)+(List.hd (List.tl prejsnji)))::prejsnji;;
fibonacci 20;;
fibonacci 60;;
(*Pazi kr online compiler overruna*)
  

let rec min_seznama=function
| h::[] -> h
| h1::h2::t when h1<h2->min_seznama (h1::t)
| h1::h2::t ->min_seznama (h2::t);;

min_seznama [1;3;2;5;12;1231;123131];;

(*funkcija explode ki string razbije na char list*)
let explode beseda=
let rec pomoznafakeforzanka seznam i =match i with
| -1 -> seznam
| x -> pomoznafakeforzanka ((beseda.[x])::seznam) (x-1) in 
pomoznafakeforzanka[] ((String.length beseda)-1);; (*V tem zadnjem koraku poveš ki zčne ta zanka*)

explode "Na zdravje!";;