let map_polje f polje=
  let novoPolje=Array.make (Array.length polje) (f polje.(0)) in
  for i=0 to (Array.length polje)-1 do
    novoPolje.(i)<- f polje.(i)
    done;
    novoPolje;;

map_polje ((+)1) [|1;2;6;10|];;

let max_polje polje=
  let trenutno_najvecji=ref polje.(0) in
  Array.iter (fun x->if (x> !trenutno_najvecji) then trenutno_najvecji:=x) polje;
  !trenutno_najvecji;;

max_polje [|2;5;3;7;3|];;

let fold_left_polje f zacetni_elm polje=
  let elm=ref zacetni_elm in
    Array.iter (fun y->elm:=f (!elm) y) polje; !elm;;

fold_left_polje (fun x y->y::x) [] [|2;6;7;3|];;

let rezine polje_elm polje_index =
  Array.map (fun x->polje_elm.(x)) polje_index;;

rezine [|'a';'b';'c';'d';'e'|] [|2;1;0;3|];;

let filter_polje f polje=
  let i=ref 0 in
  Array.iter (fun x->if (f x) then incr i) polje;
  let podpolje=Array.make (!i) polje.(0) in i:=0;
  Array.iter (fun x->if (f x) then (podpolje.(!i)<-x;incr i)) polje;
  podpolje;;

filter_polje (fun x->x>4) [|4;6;2;5;1;9;6|];;

let polje1=[|1|] in let polje2=polje1 in let polje3=Array.copy polje1 in polje1.(0)<-10; (polje1,polje2,polje3);;

let m1=[|[|1|]|] in let m2=m1 in let m3=Array.copy m1 in let m4=Array.map Array.copy m1 in m1.(0).(0)<-10; (m1,m2,m3,m4);;

let make_matrix_ij f m n =
  Array.make_matrix m n (f 0 0) |>
  Array.mapi (fun i x->Array.mapi (fun j y-> f i j) x);;

make_matrix_ij (+) 3 4;;

let sestej_matriki m1 m2 =
  let m3 = Array.map Array.copy m1 in
  m3 |> Array.mapi (fun i x->Array.mapi (fun j y->y+m2.(i).(j)) x);;
sestej_matriki [|[|2;6;7|];[|5;8|]|] [|[|2;5;7;|];[|2;6|]|];;

let bubble_sort_polje polje =
let temp=ref polje.(0) in
  for i=(Array.length polje)-1 downto 1 do
    for j=1 to i do
      if (polje.(j-1)>polje.(j)) then
      (temp:=polje.(j-1);polje.(j-1)<-polje.(j);polje.(j)<-(!temp))
    done
  done; polje;;

bubble_sort_polje [|7;3;3;6;4;2;5|];;