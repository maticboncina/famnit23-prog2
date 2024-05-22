(*map nad poljem*)
Array.map;;
let map_polje f polje=
let novo_polje=Array.make (Array.length polje) (f polje.(0)) in
for i=0 to (Array.length polje)-1 do
  novo_polje.(i)<-f polje.(i)
done; novo_polje;;
map_polje ((+)1) [|1;2;6;10|];;
map_polje float_of_int [|1;2;6;10|];;

Array.make 4 1;;
(*- : int array = [|1; 1; 1; 1|]*)

(*funckija max nad poljem*)
let max_polje polje=
let trenutno_najvecji=ref polje.(0) in
Array.iter
(fun x->if (x>(!trenutno_najvecji)) then trenutno_najvecji:=x)
polje; !trenutno_najvecji;;
max_polje [|3;2;6;1;0|];;

(*fold left nad poljem*)
let fold_left_polje f zacetni_elm polje=
let elm=ref zacetni_elm in 
Array.iter (fun y->elm:=f (!elm) y) polje; !elm;;
fold_left_polje (fun x y->y::x) [] [|1;2;4;6|];;
Array.fold_right (fun y x->y::x) [|1;2;4;6|] [];;
(*implentiraj rezine torte*)
let rezine polje_elm polje_index =
Array.map (fun x->polje_elm.(x)) polje_index;;

rezine [|'a';'b';'c';'d';'e'|] [|3;0;2;1;3;4|];;

(*array filter*)
let filter_polje f polje=
let i=ref 0 in
Array.iter (fun x->if (f x) then incr i) polje;
let podpolje=Array.make (!i) polje.(0) in i:=0;
Array.iter (fun x->if (f x) then (podpolje.(!i)<-x;incr i)) polje;
podpolje;;

filter_polje (fun x->x>4) [|4;5;2;8;10;0|];;

(*sestej 2x array ne pravoktnih matrix*)
let sestej_matriki m1 m2=
let m3=Array.map Array.copy m1 in
