(*isvezimo zapise in unije*)
type zapis={a:int;b:string};; 
{a=3;b="foo"}.a;;
type unija=Moski | Zenska | Drugo of string;;
Moski;
Drugo "neopredeljen";;

(*naloga iz izpita*)
type 'a kv_pair=string*'a;;
(("",0):int kv_pair);;
type 'a kv_array='a kv_pair array;;
([|("",0)|]:int kv_array);;
((Array.make 100 (("",0):int kv_pair)):int kv_array);;
([|("a",1);("b",-1);("c",5);("",0);("",0)|]:int kv_array);;

let kv_add (polje: 'a kv_array) (elm: 'a kv_pair)=
  let temp1=ref elm in
  let temp2=ref elm in
  for i=0 to (Array.length polje)-1 do
    if (fst (!temp1))<(fst polje.(i)) || (fst polje.(i)="") then (
      temp2:=polje.(i);
      polje.(i)<-(!temp1);
      temp1:=(!temp2)
    ) (*torej, še je manjši, končamo*)
  done; polje;;
  
kv_add ([|("a",1);("c",-1);("d",5);("",0);("",0)|]) ("b",4);;

  (*seznam na 2 nacina*)
3::4::[];;
(*osnovne funkcije za sezname: dolzina, obrni*)
type 'a seznam1=Prazen | Neprazen of 'a*'a seznam1;; 
Prazen;;
Neprazen (1, Neprazen (2, Neprazen (3, Neprazen (4, Prazen))));;
let rec dolzina_seznam1=function
  | Prazen ->0
  | Neprazen (hd,tl)->1+(dolzina_seznam1 tl);;
dolzina_seznam1 (Neprazen (1,Neprazen (2,Neprazen (3,Neprazen (4,Neprazen (5,Neprazen (6,Prazen)))))));;

type 'a seznam2=Prazen2 | Neprazen2 of 'a par and 'a par={hd:'a;tl:'a seznam2};;
Prazen2;;
Neprazen2 {hd='a';tl=Neprazen2 {hd='c';tl=Neprazen2 {hd='g';tl=Prazen2}}};;

let obrni_seznam2 seznam=
  let rec spoji_obrni s1 s2=match s1 with
    | Prazen2 ->s2
    | Neprazen2 {hd=a;tl=b} ->spoji_obrni b (Neprazen2 {hd=a;tl=s2}) in
  spoji_obrni seznam Prazen2;;
obrni_seznam2 (Neprazen2 {hd='a';tl=Neprazen2 {hd='c';tl=Neprazen2 {hd='g';tl=Prazen2}}});;

(*drevesa=seznami z več repi, na več načinov, dinamično/fiksno število repov*)
type 'a drevo3=Prazno3 | Node3 of 'a*'a drevo3*'a drevo3;;
let d1=Node3 (10,Node3 (5,Node3 (4,Prazno3,Prazno3),Node3 (8,Prazno3,Prazno3)),
              Node3 (15,Node3 (13,Prazno3,Prazno3),Node3 (20,Prazno3,Prazno3)));;

(*binarno iskalno drevo, vstavi, je_element, izpiši_po_vrsti, minimum*)
let rec vstavi_drevo elm=function
  | Prazno3->Node3 (elm,Prazno3,Prazno3)
  | Node3 (hd,ltl,rtl) when elm>hd->Node3 (hd,ltl,vstavi_drevo elm rtl)
  | Node3 (hd,ltl,rtl) when elm<hd->Node3 (hd,vstavi_drevo elm ltl, rtl)
  | x->x;; 
[|10,5,4,8,15,13,20|] |> Array.fold_left (fun x y-> vstavi_drevo y x) Prazno3;;

let rec izpisi_po_vrsti=function
  | Prazno3-> ()
  | Node3 (hd,ltl,rtl)->(izpisi_po_vrsti ltl);print_int hd;print_string "\n";(izpisi_po_vrsti rtl);;
let d1=Node3 (10,Node3 (5,Node3 (4,Prazno3,Prazno3),Node3 (8,Prazno3,Prazno3)),
              Node3 (15,Node3 (13,Prazno3,Prazno3),Node3 (20,Prazno3,Prazno3)));;
izpisi_po_vrsti d1;;

(*drevo z vec repi*)
type 'a drevo4=Prazno4 | Node4 of 'a*('a drevo4) list;;
Prazno4;;
Node4 (6,[Node4 (6,[Node4 (1,[])]);Node4 (4,[Node4 (6,[]);Node4 (10,[])]);Node4 (1,[])]);;
let rec sestej_drevo4=function
  | Prazno4->0
  | Node4 (hd,seznam_otrok)->hd+(List.fold_left (fun x y->
      0+(sestej_drevo4 y)
    ) 0 seznam_otrok);;
Node4 (6,[Node4 (6,[Node4 (1,[])]);Node4 (4,[Node4 (6,[]);Node4 (10,[])]);Node4 (1,[])]) |> sestej_drevo4;;


