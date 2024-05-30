(* primer s funktorji: elenebti; urejen seznam*)
module type ELM=sig
  type t 
  val init : string -> int -> t
  val greater : t -> t -> bool
  val to_string : t -> string
end;;


module Par:ELM=struct
  type t={key:string; mutable value:int}
  let init k v={key=k;value=v}
  let greater x y=x.key>y.key
  let to_string x = Printf.sprintf "{%s,%i}" x.key x.value
end;;

module UrejenSeznam:(functor (Elm : ELM) ->
  sig
    type t
    val init : unit -> t
    val insert : Elm.t -> t -> t
    val print : t -> unit
  end)=functor (Elm:ELM) -> struct
  type t={mutable seznam:Elm.t list}
  let init ()={seznam=[]}
  let insert abselm abssez =
    let rec vstavi elm seznam = match seznam with 
      |[] -> [elm]
      | h::t when Elm.greater h elm -> elm::h::t
      | h::t -> h::(vstavi elm t) in
    abssez.seznam <- vstavi abselm abssez.seznam; abssez
  let print abssez=List.iter (fun x-> Printf.printf "%s " (Elm.to_string x)) abssez.seznam
end;;

module UrejenSeznamParov=UrejenSeznam (Par);;

let usp1=UrejenSeznamParov.init ();;
UrejenSeznamParov.insert (Par.init "g" 3) usp1;;
UrejenSeznamParov.insert (Par.init "b" 4) usp1;;
UrejenSeznamParov.insert (Par.init "k" 1) usp1;;
UrejenSeznamParov.print usp1;;