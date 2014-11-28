(* Problem number 3 of Euler Project
 * Find the maximun prime factor of a given number
 * Programmed by: Carlos Montilla
 *)

fun isDivisible (value: IntInf.int, divisor: IntInf.int) =
    value mod divisor = 0


fun findDivisor (num: IntInf.int) : IntInf.int =
    if num = 1 then Int.toLarge 1
    else
        let fun helper (num: IntInf.int, divisor: IntInf.int) =
                if (isDivisible(num, divisor)) then divisor
                else helper(num, (divisor+1))
        in
            helper(num, Int.toLarge 2)
        end



fun findMaxPrimeFactor (num: IntInf.int) : IntInf.int =
    if num = 1 then 1
    else
        let
            val divisor = findDivisor(num)
            val maxRemaining = findMaxPrimeFactor((num div divisor))
        in
            if divisor > maxRemaining then divisor else maxRemaining
        end

val test = (Int.toLarge 13195)
val  twenty_nine = findMaxPrimeFactor test;
val ceroLarge = Int.toLarge 0;
val value = ceroLarge + 600851475143;
val test1 = findMaxPrimeFactor value;
