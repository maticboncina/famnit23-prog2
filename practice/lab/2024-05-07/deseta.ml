class denar vr va=object (self)
  val mutable vrednost:int=vr
  val valuta:string=va
  method get_vr=vrednost
  method get_va=valuta
  method set_vr elm=vrednost<-elm
end;;

(new denar 3 "EUR")#get_va;;

class denarnica=object
  val mutable seznam:denar list=[]
  method dodaj vr va=seznam<-(new denar vr va)::seznam
  method print=List.map (fun x->(x#get_vr, x#get_va)) seznam
end;;

let d1=new denarnica;;
d1#dodaj 3 "EUR";;
d1#dodaj 100 "SIT";;
d1#print;;

class tocka x y=object
  val lokacija:float*float=(x, y)
  method get_lok=lokacija
end;;

class krog x y polmer=object
  inherit tocka x y as super
  val r:float=polmer
  val pi=3.14159
  method obseg=2.*.pi*.r
  method ploscina=pi*.r*.r
  method get_lok=(4.5, snd super#get_lok)
end;;

let k1=new krog 0. 0. 10.;;
k1#obseg;;
k1#ploscina;;
k1#get_lok;;

((new krog 1.2 2.1 10.):>tocka)#get_lok;;
[new tocka 2. 3.;((new krog 1.2 2.1 10.):>tocka)] |> List.map (fun x->x#get_lok);;

class virtual stirikotnik a b c d=object
  val mutable dolzine=(a,b,c,d)
  method obseg=match dolzine with (a,b,c,d)->a+.b+.c+.d
  method virtual ploscina:float
end;;

(new stirikotnik 2 4 3 5);;

class pravokotnik a b=object
  inherit stirikotnik a b a b as super
  method ploscina=match dolzine with (a,b,c,d) -> a*.b
end;;

(new pravokotnik 3. 4.)#ploscina;;
(new pravokotnik 3. 4.)#obseg;;

class kvadrat a=object
  inherit pravokotnik a a as super
end;;

(new kvadrat 5.)#ploscina;;
(new kvadrat 5.)#obseg;;

[|((new kvadrat 3.):>stirikotnik);((new pravokotnik 3. 4.):>stirikotnik)|] |> Array.map (fun x->x#ploscina);;

class valj v r=object
  val visina:float=v
  inherit krog 0. 0. r as krog
  method osnovna_ploskev_ploscina=krog#ploscina
  method volumen=krog#ploscina*.visina
  inherit pravokotnik 0. 0. as plasc
  initializer dolzine<-(visina,krog#obseg,visina,krog#obseg)
  method povrsina=krog#ploscina*.2.+.plasc#ploscina
end;;

let valj1=new valj 10. 1.;;
valj1#povrsina;;
valj1#volumen;;

valj1#ploscina;;
valj1#obseg;;
(new pravokotnik 10. (new krog 0. 0. 1.)#obseg)#ploscina;;
(new pravokotnik 10. (new krog 0. 0. 1.)#obseg)#obseg;;

class virtual ['a] primer_poli_dedovanja pf=object
  val mutable seznam:'a list=[]
  val printfun:'a -> unit =pf
  method virtual insert: 'a->unit
  method print=List.iter (fun x->printfun x) seznam; print_endline ""
end;;

class int_seznam=object
  inherit [int] primer_poli_dedovanja print_int as super
  method insert elm=seznam<-elm::seznam
end;;

let is1=new int_seznam;;
is1#insert 1;;
is1#insert 3;;
is1#insert 4;;
is1#insert 2;;
is1#print;;