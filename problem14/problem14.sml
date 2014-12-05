(* Problem 14 of project euler
 * Find the largest collatz's sequence for n_0 between 2 and 1e6
 * Programmed by: Carlos Montilla *)

(* Returns the collatz sequence for a given starting number *)
fun collatz (n0: IntInf.int) : IntInf.int list =
    let fun next_number ni =
	    if ni mod 2 = 0 then ni div 2 else ni * 3 + 1
	fun helper ni acc =
	    if ni = 1 then acc
	    else let val n_value = next_number ni
		 in
		     helper n_value [n_value] @ acc
		 end
    in
	helper n0 [n0]
    end

(* Returns a pair with the number thar produces the largest chain
 * and the value of the length of the chain
 * It does not work with arguments less than 1*)
fun maxCollatzLength rng =
    if rng = 1 then (1, 1)
    else
	let val max_value = length (collatz (Int.toLarge rng));
	    val (max_range, n') = maxCollatzLength (rng - 1);
	in
	    if max_value > max_range then (max_value, rng)
	    else (max_range, n')
	end

val rng = 1000000
val (result, n) = maxCollatzLength rng;
print ("n0 = " ^ (Int.toString n) ^ " Produces a chain of length = " ^ (Int.toString result) ^ "\n");
