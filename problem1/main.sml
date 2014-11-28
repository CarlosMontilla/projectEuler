(*Problem number 1 of Euler Project
 * Find the sum of all natural numbers
 * below 100 that are multiple of 3 or 5
 *)

(*Programmed by: Carlos Montilla*)


(*Calcules is the number value is multiple of mult*)
fun multiple mult value =
    value mod mult = 0

(*Creates a list of integer between init and final (final exclusive)*)
fun range init final =
    let fun helper init final acc =
            if init = final then acc
            else helper (init+1) (final) (acc @ [init])
    in
        helper init final []
    end

fun sum x =
    case x of
        [] => 0
      | x::xs =>  x + sum xs

val lst = range 1 1000
fun multiple3 x = multiple 3 x;
fun multiple5 x = multiple 5 x;
fun  multiple3or5 x = multiple3 x orelse multiple5 x;


val multiples = List.filter multiple3or5 lst;
val suma = sum multiples
