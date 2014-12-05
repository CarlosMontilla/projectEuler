(* Problem number 6 of project Euler
 * Programmed by: Carlos Montilla *)


fun sumFuncRange func min max =
    let
	fun helper min max acc =
	    let
		val next_value = func min;
	    in
		if min > max then acc else helper (min + 1) max (acc + next_value)
	    end
    in
	helper min max 0
    end


val min_value = 1;
val max_value = 100;

val sumN = sumFuncRange (fn x => x) min_value max_value;
val squaredSum = sumN * sumN;
val sumNsquared = sumFuncRange (fn x => x*x) min_value max_value;
val result = squaredSum - sumNsquared;
print ((Int.toString result) ^ "\n");
