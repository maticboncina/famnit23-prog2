print_string "Pozdravljen svet!\n";;
(* Quitneš iz terminala z #quit;; *)
(* run .ml files with ocamlc -o hellworld hellowolrd.ml - rabiš subsystem iz slajdov. Ko je skompilan poženeš z imenom*)
(* osnovni tipi*)
1;;
"string";;
'c';
2.;; (*float*)
false;; (*bollean*)

(*sestevanje*)
4+5;;
(+) 4 6;;
(+);;

4. +. 2.5;;
(+.);; (*sestevanje s floati*)
1.1234e+4;;

(* mnozenje - nujni presledki *)
( * ) 4 6;;

(* logika *)
(>);;
3 > 4;;
3.0 > 2.0;;
'c' > 'a';;

2 <= 0;;
2 = 4;;
2!=2;;
2<>2;;
true || false;; (*and*)
true && false;; (*or*)

int_of_char 'c';;
int_of_char 'a';;

(*negacija*)
not (123=0);;

int_of_float 2.4;;
int_of_float (-2.4);; (*odseka decimlano vejico*)
13/5;; (*ocaml vedno zaokrožuje proti ničli*)

string_of_int 10;;
(*char to strung*)
Char.escaped 'a';

(*string to cahar*)
"nizbesed".[0];;
"nizbesed".[1];;
"nizbesed".[2];;

(*funkcje*)
(fun x->x+1) 4;;
4 |> (fun x->x+1);;

(*pretvori v veliko začetnico*)
's' |> int_of_char |> (fun x->x-32) |> char_of_int;;

(*potence - deluje samo za floate, ker gre prehitro v overflow*)
( ** );;

(*korenjenje*)
64. ** 0.5;;

(*if stavek*)
if (3<1) then "bla" else "stop";;
(*izhoda morata biti istega tipa?*)

(* show + tip *)
#show String;;
String.length;;

(*concatinate*)
"ime"^"Priimen";;

(*dva/tri/večterice*)
(3,"banana");;
(3, "banana", 4.5, 'c');;

fst(3,5);;
snd(3,5);;

(*seznami*)
[];;
[4;7;3];;
[4.3;4.6];;
[[];[1;4]];;

(*PAZI SEZNAM PAROV*)
[1,3];
[(1,3);(3,5)];;

(*SEZNAM*)
List.hd [3;4;6];;
List.tl [3;4;6];;

3::[4;5;6;7];;

1::2::3::[];;
1::[2;3];;

List.tl ["banana"];;
List.length ["banana"];;
List.length [];;

(*in pa and*)
let f=fun x->x+1;;
let a=19;;
f a;;
let a=10 and b=5 in let c=a+b in c*3;;
(* and naredi, da lahko narediš v dveh korakih in spoustiš let pred b*)
(* in ("v komandi" (SLO)) ... *)