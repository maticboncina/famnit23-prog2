(* Obtoječi moduli, #show, uporaba funkcij, open*)
#show List;;
#show Int64;;
Int64.zero;;
List.hd [1;2;3];;
let open Printf in printf "foo\n";;
(*Int64, primer seštevanja *)
Int64.zero;;
(2. ** 31. |> int_of_float)-1;;
21474836470L;;
Int64.add 3L 5L;;

(*primer custom modula, ime z veliko začetnico, spremenljivke in fukncije*)

module ImeModula=struct
  let pi=3.14 
  let funkcija () = print_endline "foo"
  let ni_funkcija=print_endline "banana"
end;;
ImeModula.pi;;
ImeModula.funkcija ();;
ImeModula.ni_funkcija;;
#show ImeModula;;

(*bedasti sklad*)

module BedastiSklad=struct
  let data=ref []
  let push elm=data:=elm::(!data)
  let pop ()=match !data with h::t -> data:=t;h | []->failwith "prazen sklad"
end;;
BedastiSklad.push 1;;
BedastiSklad.push 2;;
BedastiSklad.pop () ;;
BedastiSklad.pop () ;;
BedastiSklad.pop () ;;

(* tipi, skrivanje funkcij, vmesnik: tip/podpis modula, primer polozaji *)
module Polozaj=struct
  type t=Kralj |Papez |Vitez |Kmet
  let cin=function | Kralj->4 | Papez->3 |Vitez->3 |Kmet->1
  let (>>>) x y=(cin x)>(cin y)
end;;
(*Papez;; ne dela*)
(Papez:Polozaj.t);;
Polozaj.Vitez;;
let open Polozaj in Kralj;;
Polozaj.cin Polozaj.Vitez;;
let open Polozaj in Kralj>>>Kmet;;
Polozaj.(>>>) Polozaj.Vitez (Papez:Polozaj.t);;

module Polozaj2:sig
  type t = Kralj | Papez | Vitez | Kmet 
  val ( >>> ) : t -> t -> bool
end=struct
  type t=Kralj |Papez |Vitez |Kmet
  let cin=function | Kralj->4 | Papez->3 |Vitez->3 |Kmet->1
  let (>>>) x y=(cin x)>(cin y)
end;;

let open Polozaj2 in Kralj>>>Kmet;;
(*Polozaj2.cin Polozaj2.Vitez;; smo skrrili cin in ga ne moremo poklicati*)

module type POLOZAJ=sig
  type t = Kralj | Papez | Vitez | Kmet 
  val ( >>> ) : t -> t -> bool
end;;

module Polozaj3:POLOZAJ=struct
  type t=Kralj |Papez |Vitez |Kmet
  let cin=function | Kralj->4 | Papez->3 |Vitez->3 |Kmet->1
  let (>>>) x y=(cin x)>(cin y)
end;;

#show Polozaj3;;

#show POLOZAJ;;

module Polozaj4=struct 
  include (Polozaj:POLOZAJ)
end;;

(*abstraktni tipt, primer kompleksna števila*)
module Kompleksna:sig
  type t
  val abs : t -> float
  val add : t -> t -> t
  val to_string : t -> string
  val init : float -> float -> t
end=struct
  type t={mutable re:float; mutable im:float}
  let abs z= sqrt(z.re ** 2. +. z.im ** 2.)
  let add z1 z2={re=z1.re +. z2.re;im=z1.im+.z2.im}
  let to_string z=Printf.sprintf "%f+%fi" z.re z.im
  let init a b={re=a;im=b}
end;;
Kompleksna.init 3. 4.;; 
Kompleksna.init 3. 4. |> Kompleksna.to_string;;
Kompleksna.init 3. 4. |> Kompleksna.abs;;
Kompleksna.add (Kompleksna.init 3. 4.) (Kompleksna.init 2. 8.) |> Kompleksna.to_string;;

(* polimorfični moduli (v resnici samo poli tip), sklad narejen za prav*)

module Sklad:sig
  type 'a t
  val init : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a
end=struct
  type 'a t={mutable data:'a list}
  let init ()={data=[]}
  let push elm sklad=sklad.data<-elm::sklad.data
  let pop sklad=match sklad.data with h::t->sklad.data<-t;h | _->failwith "prazen sklad"
end;;
let s1=Sklad.init ();;
let s2=Sklad.init ();;
Sklad.push 1 s1;
Sklad.push 2 s1;
Sklad.pop s1;;
Sklad.pop s1;;
Sklad.pop s1;;

Sklad.push 'a' s2;
Sklad.push 'b' s2;
Sklad.pop s2;;
Sklad.pop s2;;
Sklad.pop s2;;


(* dodajanje k obstoječem modulu, include *)
module Foo=struct
  include (List:sig 
             val iter : ('a -> unit) -> 'a list -> unit
             val hd : 'a list -> 'a
           end)
  let pi=3.14
  let add=(+)
end;;

(*osnove funktorjev, na dva nacina *)
let f x=x+1;;
let g=function x->x+1;;

module F (X:sig 
    val iter : ('a -> unit) -> 'a list -> unit
    val hd : 'a list -> 'a
  end)=struct include X end;;

module PrimerF=F (List);;
module PrimerF2= F (Foo);;


module G=functor (X:sig 
                    val iter : ('a -> unit) -> 'a list -> unit
                    val hd : 'a list -> 'a
                  end) -> struct include X end;;

module PrimerG=G (List);;
module PrimerG2=G (Foo);;
PrimerG.hd [1;2];;











