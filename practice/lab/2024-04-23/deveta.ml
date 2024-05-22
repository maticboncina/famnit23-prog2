(* class, object, end, new *)
class beden=object end;;
let b1=new beden;;
(* val, method*)
class beden=object
  val a=0
  method get_a=a
end;;
(new beden)#get_a;;
(* to_string, Printf *)
#show Printf;;
Printf.printf "nekaj\n";;
Printf.sprintf "nekaj\n";;
Printf.sprintf "%d nekaj\n" 10;;
Printf.sprintf "%d nekaj %d\n" 10 4;;
Printf.sprintf "%d %s %c %f %b nekaj\n" 10 "niz" 'a' 3.4 true;;
class beden=object
  val a=0
  val b="niz"
  val c=false
  method to_string=Printf.sprintf "v objektu imamo %d %s %b" a b c
end;;
(new beden)#to_string;;
(* parametri, set, tipi*)
class oseba (a:string) b=object 
  val ime=a
  val priimek:string=b
  method get_ime=ime
end;;
let o1=new oseba "Janez" "Novak";;
o1#get_ime;;
(*sklad*)
class sklad=object 
  val mutable podatki:int list=[]
  method push elm=podatki<-(elm::podatki)
  method pop=let glava=List.hd podatki in
    podatki<-List.tl podatki;
    glava
end;;
let s1=new sklad;;
s1#push 2;;
s1#push 1;;
s1#pop;;
s1#push 3;;
s1#pop;;
(* polimorfizem *)
class ['a,'b] primer_polimorfizma a b=object
  val par:'a*'b=(a,b)
  method get_par=par
end;;
(* poli sklad*)
class ['a] sklad=object 
  val mutable podatki:'a list=[]
  method push elm=podatki<-(elm::podatki)
  method pop=let glava=List.hd podatki in
    podatki<-List.tl podatki;
    glava
end;;
let s2=new sklad;;
s2#push 4;;
let s3=new sklad;;
s3#push "foo";;

(*self, private*)
class primer_privatnih=object (self)
  val a=0
  val b=10
  method private vsota=a+b 
  method to_string=Printf.sprintf "skupaj je %d" self#vsota 
end;;
(new primer_privatnih)#to_string;;
(* (new primer_privatnih)#vsota;; ne dela ker je privatna metoda*)
(*inicializator*)
class primer_inicializatorja=object
  val mutable a=0
  initializer a<-a+1;Printf.printf "ustvarili smo nov objekt\n z vrednostjo %d" a 
end;;
new primer_inicializatorja;;
(* odmor do 14:22*)
(*sklad izpopolnjen*)
class ['a] sklad=object 
  val mutable podatki:'a list=[]
  method push elm=podatki<-(elm::podatki)
  method pop=let glava=List.hd podatki in
    podatki<-List.tl podatki;
    glava
  method empty=podatki=[]
  method obrni=podatki<-List.rev podatki
end;; 
class ['a] vrsta=object 
  val mutable prvi:'a sklad=new sklad (*  vstavjamo*)
  val mutable drugi:'a sklad=new sklad (* jemljemo *)
  method vstavi elm=prvi#push elm
  method vzami=if drugi#empty then (prvi#obrni;drugi<-prvi; prvi<-new sklad); drugi#pop
end;;
let v1=new vrsta;;
v1#vstavi 1;;
v1#vstavi 2;;
v1#vzami;;
v1#vstavi 3;;
v1#vzami;;


(*agregacija   method vzami=if drugi#empty then (drugi<-prvi#obrni; prvi<-new sklad); drugi#pop*)

class izpitna_naloga a b c=object
  val s_t:int=a
  val d_t:int=b
  val tez:int=c
  method get_s_t=s_t
  method get_d_t=d_t
  method get_tez=tez
  method to_string=Printf.sprintf
  "pri izpitu s težavnostjo %d je doseggel %d/%d tock" tez d_t s_t
end;;

new izpitna_naloga 100 50 1;;
let seznam_izpitov=Array.make 10 0 |> Array.fold_left (fun x y ->(new izpitna_naloga (100) (Random.int 100) (Random.int 3)) ::x)[];;

seznam_izpitov |> List.map (fun x->x#to_string);;
(*vrsta*)

(*primer: izpitna_naloga*)





