(* Problem number 15 of project Euler
 * Find how many routes are connecting the top left vertex
 * with the bottom right vertex in a 20x20 grid
 * allowing just moves to the right and to the bottom
 * Programmed by Carlos Montilla
*)


fun create_initial_vector n =
    let
	fun helper n acc =
	    if length acc = n then acc else helper n (acc @ [Int.toLarge 1])
    in
	helper n [Int.toLarge 1]
    end


fun calculate_next_line (lst: IntInf.int list) : IntInf.int list =
    let
	fun helper acc =
	    if length acc = length lst then acc
	    else
		let val next_value = List.last acc + List.nth (lst, length acc)
		in helper (acc @ [next_value])
		end
    in
	helper [hd lst]
    end

fun calculate_routes x_points y_points =
    let
	fun helper current_line last_vector =
	    if current_line > y_points then last_vector
	    else helper (current_line + 1) (calculate_next_line last_vector)
    in
	List.last (helper 2 (create_initial_vector x_points))
    end


val grid = 21 (* a 20x20 grid has 21x21 points in each direction *)
val result = calculate_routes grid grid
