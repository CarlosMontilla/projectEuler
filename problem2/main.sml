(* Problem number 2 of Euler Project
 * Find the sum of all even number in Fibonacci
 * sequence that are below 4 millons
 *)


fun sum xs =
    let fun helper xs acc =
        case xs of
            [] => acc
          | x::xs' => helper xs' (x + acc)
    in
        helper xs 0
    end

fun fibonacciLessThan maxValue =
    let
        fun helperLessThan value acc =
            let
                fun nextNumber x1 x2 =
                    x1 + x2

                val next = nextNumber (List.last acc) (List.nth (acc, (List.length acc) - 2))
            in
                if next > value then acc
                else helperLessThan value (acc @ [next])
            end
    in
        case maxValue of
            1 => []
          | 2 => [1]
          | 3 => [1, 2]
          | x => helperLessThan x [1, 2]
    end


fun isEven x =
    x mod 2 = 0

val lst = fibonacciLessThan 4000000;
val evenNumber = List.filter isEven lst
val suma = sum evenNumber;
print ((Int.toString suma) ^ "\n");
