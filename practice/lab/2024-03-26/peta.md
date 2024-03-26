(*več vrstični ukaz*)

(*referenca*)
let a=ref 0;;
a:=4;;
!a;;
a:=!a+1;;
!a;;

(*ista stvar samo v eni vrstici*)
let a=ref 0 in
a:=4;
!a |> ignore;
a:=!a+1;
!a;;

(*podpičje se da samo za stvarmi ki vrnejo unit*)

(*referenca je lahko tudi kompleksna struktura*)
let seznam=ref [1;2;3] in seznam:=0::(!seznam); !seznam;;

(*pri referencah pazi na aliase*)
let a=ref 0 in
let b=a in
incr a; (*a.=!a+1;;*)
!b;;

(*while do done*)
let i=ref 1 in
while(!i<=5) do print_int (!i); print_string "\n";i:=2*(!i); done;;

(*naloga*)
let delitelji n=
  let seznam=ref [] in
  let i=ref n in
  while (!i>=1) do if (n mod (!i) = 0) then seznam:=(!i)::(!seznam); decr i done;!seznam;;
  
(*for zanka*)
for i=1 to 3 do print_int i; print_string "\n" done;;
for i=3 downto 1 do print_int i; print_string "\n" done;;

(* narisi slikico: kvadrat, pesceno uro, trikotnik*)
let kvadrat n=
  for i=1 to n do
    for j=1 to n do
      if(i=1 || i=n || j=1 || j=n) then print_string "*" else print_string" "
    done; print_string "\n";
  done;;
  
(*polje=arrays*)
let p1=[|1;2;3;4|];;
p1.(0);;
p1.(1);;
p1.(2)<-10;;
p1;;
Array.length p1;;
Array.make 6 "bla";;

(*naloga 2*)
let od_n_do_m n m=
  let polje = Array.make ((max m n)-(min m n)+1) 0 in
  if (n<=m) then
    for i=n to m do polje.(i-n)<-i done
  else
    for i=n downto m do polje.(n-i)<-i done;
  polje;;
  
(*zdruzevanje dveh polj*)
let zdruzi_polje polje1 polje2=
  let novopolje =Array.make ((Array.length polje1)+(Array.length polje2)) (polje1.(0)) in
    for i=0 to (Array.length polje1)-1 do
      novopolje.(i)<-polje1.(i)
    done;
    for i=0 to (Array.length polje2)-1 do
      novopolje.(i+(Array.length polje1))<- polje2.(i)
    done; novopolje;;
    
(*kopiranje polj*)
let m1=[|1;3;5|];;
let m2=m1;;
let m3=Array.copy m1;;
m1.(0)<-10;;
m2;;
m3;;

(*random int*)
Random.int 5;;

(* za dan array in dano dolzino naredi random array dane dolzine z vrednostmi iz vhodnega arraya*)
[|'a';'b';'d';'e';'f'|].(Random.int 5);;
let random_array polje n=
  let novopolje=Array.make n (polje.(0)) in
  let staradolzina=Array.length polje in
  Array.map (fun x->polje.(Random.int (staradolzina))) novopolje;;
  
(*iz seznama v polje*)
let polje_of_seznam seznam=
  let polje=Array.make (List.length seznam) (List.hd seznam) in
  let rec pomozna sez i=match sez with
  | []-> seznam
  | h::t-> polje.(i)<-h; pomozna t (i+1) in
  pomozna seznam 0;;
  
(*matrike*)
let m1=Array.make_matrix 2 3 4;;
m1.(0).(0);;
Array.length m1;;
Array.length m1.(0);;
m1.(0).(0)<-100;;
m1;;

(*transponiranje matrik*)
let transpose matrika=
  let novamatrika=Array.make_matrix
    (Array.length matrika.(0)) (Array.length matrika) (matrika.(0).(0)) in
    
    for i=0 to (Array.length matrika.(0))-1 do
      for j=0 to (Array.length matrika)-1 do
      novamatrika.(i).(j)<-matrika.(j).(i)
      done
    done; novamatrika;;
    
(*print matrika*)
let print_matrix funkcija_za_printanje matrika=
  Array.iter (fun x->Array.iter
  (fun y->funkcija_za_printanje y; print_string " ") x; print_string "\n") matrika;;