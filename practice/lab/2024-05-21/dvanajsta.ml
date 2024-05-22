    (* PRVA NALOGA *)
    (* 4. naloga iz izpita *)
module type MatrixSig=sig
  type 'a t
  val create : int * int -> 'a -> 'a t
  val get : 'a t -> int * int -> 'a
  val set : 'a t -> int * int -> 'a -> 'a t
  (* val rotate90 : 'a t -> 'a t (ker pravijo navodila da ga skrijemo) *)
end;;

module Matrix=struct
  type 'a t={mutable mat:'a array array; mutable row:int; mutable col:int}
  let create (r,c) elm={mat=Array.make_matrix r c elm; row=r;col=c}
  let get abstip (i,j)=abstip.mat.(i).(j)
  let set abstip (i,j) elm=abstip.mat.(i).(j)<-elm; abstip
  let rotate90 abstip=let novamatrika=Array.make_matrix abstip.col abstip.row abstip.mat.(0).(0) in
    for i=0 to abstip.row-1 do
      for j=0 to abstip.col-1 do
        novamatrika.(j).(abstip.row-1-i)<-abstip.mat.(i).(j)
      done
    done; abstip.mat<-novamatrika; let temp=abstip.col in abstip.col<-abstip.row; abstip.row<-temp; abstip
end;;

let m1=Matrix.create (3,2) 1;;
Matrix.set m1 (0,1) 2;;
Matrix.set m1 (1,0) 3;;
Matrix.set m1 (1,1) 4;;
Matrix.set m1 (2,0) 5;;
Matrix.set m1 (2,1) 6;;
Matrix.rotate90 m1;;

module Matrix2=(Matrix:MatrixSig);;

let m1=Matrix2.create (3,2) 1;;
Matrix2.set m1 (0,1) 2;;
Matrix2.set m1 (1,0) 3;;
Matrix2.set m1 (1,1) 4;;
Matrix2.set m1 (2,0) 5;;
Matrix2.set m1 (2,1) 6;;
(* Matrix2.rotate90 m1;; (ker smo skrili rotate, to ne dela) *)



    (* DRUGA NALOGA *)
    (* prenos tipa me drazličnimi moduli: with type X.t *)
module IntFoo=struct
  type t=int
  let create elm=elm
  let get abstip=abstip
end;;

module type INTFOO=sig
  type t
  val create : int -> t
  val get : t -> int
end;;

module If1=(IntFoo:INTFOO);;
module If2=(IntFoo:INTFOO with type t=IntFoo.t);;
module If3=(IntFoo:INTFOO with type t=IntFoo.t);;
let if1=If1.create 4;;
(* If2.get if1;; (ne dela, ker sta tipa If1.t in If2.t različna) *)
If2.create 0 |> If3.get;; (* dela, ker sta tipa If2.t in If3.t enaka. To dosežemo z "with type" *)



      (* TRETJA NALGA *)
      (* ocena, profesor, student *)
module Ocena=struct
  type t={ime:string;mutable ocena:int}
  let init i o={ime=i;ocena=o}
  let spremeni o abstip=abstip.ocena<-o; abstip
  let poglej abstip=(abstip.ime, abstip.ocena)
end;;

module type STUDENT=sig
  type t
  val poglej : t -> string * int
end;;

module type PROFESOR=sig
  type t
  val poglej : t -> string * int
  val init : string -> int -> t
  val spremeni : int -> t -> t
end;;

module Student=(Ocena:STUDENT with type t=Ocena.t);;
module Profesor=(Ocena:PROFESOR with type t=Ocena.t);;

Profesor.init "Prog2" 6 |> Profesor.spremeni 7;;
Profesor.init "Prog2" 6 |> Profesor.spremeni 7 |> Student.poglej;;