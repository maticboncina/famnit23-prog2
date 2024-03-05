fun x->x+1;;
function x->x+1;;

(fun x->x+1) 4;;

let f1 = fun x->x+1;;
let f2 = function x->x+1;;
let f3 x=x+1;;

f1 2;;

(*funckija dveh sprememljivk*)

fun x->fun y-> x+y;;
function x->function y-> x+y;;
fun x y -> x+y;;
(* function x y -> x+y;; ne dela *)

let g1=fun x y->x+y;;
g1 1 3;;

let g2 x y = x+y;;

let g3 x=fun y->x+y;;

g3 1 3;;

(*pri ujemanju vzorcev moramo uporabiti function in ne fun*)

int_of_float;;
let int_of_bool = function true->1 | false ->0;;
int_of_bool true;;
int_of_bool false;;

let bool_of_int = function 0->false | x->true;;
bool_of_int 0;;
bool_of_int 3;;

(* nekej nioevga *)

(((fun x->(fun y->(fun z->(fun x->fun y->x+y) 4) 5)) 3) 4) 5;;

(*beta redukcija*)
(fun x->fun y ->x-y) (-3)5;;
