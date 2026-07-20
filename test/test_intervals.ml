module TestIntervals = struct
  open Abstract_domains.Intervals
  (* Helper per la stampa leggibile degli intervalli *)
  let bound_to_string = function
    | NegInf -> "-inf"
    | PosInf -> "+inf"
    | Int n -> string_of_int n

  let interval_to_string = function
    | Bottom -> "Bottom"
    | Interval (a, b) ->
        Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

  let run_all_tests () =
    Printf.printf "=========================================\n";
    Printf.printf "       TEST INTERPRETE INTERVALLI       \n";
    Printf.printf "=========================================\n\n";

    (* --- TEST LEQ --- *)
    Printf.printf "===== TEST leq =====\n";
    let check_leq name a b expected =
      let result = leq a b in
      let ok = result = expected in
      Printf.printf "[%s] leq (%s) = %b (atteso %b)\n"
        (if ok then " OK " else "FAIL") name result expected
    in
    check_leq "Bottom, [1, 2]" Bottom (abstract_range 1 2) true;
    check_leq "[1, 2], Bottom" (abstract_range 1 2) Bottom false;
    check_leq "[2, 3] in [1, 5]" (abstract_range 2 3) (abstract_range 1 5) true;
    check_leq "[1, 5] in [2, 3]" (abstract_range 1 5) (abstract_range 2 3) false;
    check_leq "[1, 2] in [1, 2]" (abstract_range 1 2) (abstract_range 1 2) true;
    check_leq "[0, 10] in Top" (abstract_range 0 10) top true;
    check_leq "Top in [0, 10]" top (abstract_range 0 10) false;
    print_newline ();

    (* --- TEST LUB --- *)
    Printf.printf "===== TEST lub =====\n";
    let check_lub name a b expected =
      let res = lub a b in
      let ok = res = expected in
      Printf.printf "[%s] lub(%s) -> %s (atteso %s)\n"
        (if ok then " OK " else "FAIL") name 
        (interval_to_string res) (interval_to_string expected)
    in
    check_lub "[1, 3], [5, 7]" (abstract_range 1 3) (abstract_range 5 7) (abstract_range 1 7);
    check_lub "[2, 5], [1, 3]" (abstract_range 2 5) (abstract_range 1 3) (abstract_range 1 5);
    check_lub "Bottom, [1, 2]" Bottom (abstract_range 1 2) (abstract_range 1 2);
    check_lub "[1, 2], Top" (abstract_range 1 2) top top;
    print_newline ();

    (* --- TEST GLB --- *)
    Printf.printf "===== TEST glb =====\n";
    let check_glb name a b expected =
      let res = glb a b in
      let ok = res = expected in
      Printf.printf "[%s] glb(%s) -> %s (atteso %s)\n"
        (if ok then " OK " else "FAIL") name 
        (interval_to_string res) (interval_to_string expected)
    in
    check_glb "[1, 5] e [3, 8]" (abstract_range 1 5) (abstract_range 3 8) (abstract_range 3 5);
    check_glb "[1, 3] e [5, 8] (disgiunti)" (abstract_range 1 3) (abstract_range 5 8) Bottom;
    check_glb "[1, 5] e Bottom" (abstract_range 1 5) Bottom Bottom;
    check_glb "[1, 5] e Top" (abstract_range 1 5) top (abstract_range 1 5);
    print_newline ();

    (* --- TEST SOMMA --- *)
    Printf.printf "===== TEST sum =====\n";
    let check_sum name a b expected =
      let res = sum a b in
      let ok = res = expected in
      Printf.printf "[%s] sum(%s) -> %s (atteso %s)\n"
        (if ok then " OK " else "FAIL") name 
        (interval_to_string res) (interval_to_string expected)
    in
    check_sum "[1, 2] + [3, 4]" (abstract_range 1 2) (abstract_range 3 4) (abstract_range 4 6);
    check_sum "[-2, 5] + [10, 20]" (abstract_range (-2) 5) (abstract_range 10 20) (abstract_range 8 25);
    check_sum "[1, 2] + Bottom" (abstract_range 1 2) Bottom Bottom;
    check_sum "[1, 2] + Top" (abstract_range 1 2) top top;
    print_newline ();

    (* --- TEST NEGAZIONE --- *)
    Printf.printf "===== TEST negate =====\n";
    let check_negate name a expected =
      let res = negate a in
      let ok = res = expected in
      Printf.printf "[%s] negate(%s) -> %s (atteso %s)\n"
        (if ok then " OK " else "FAIL") name 
        (interval_to_string res) (interval_to_string expected)
    in
    check_negate "[1, 5]" (abstract_range 1 5) (abstract_range (-5) (-1));
    check_negate "[-3, 2]" (abstract_range (-3) 2) (abstract_range (-2) 3);
    check_negate "Bottom" Bottom Bottom;
    print_newline ();

    (* --- TEST MOLTIPLICAZIONE --- *)
    Printf.printf "===== TEST mul =====\n";
    let check_mul name a b expected =
      let res = mul a b in
      let ok = res = expected in
      Printf.printf "[%s] mul(%s) -> %s (atteso %s)\n"
        (if ok then " OK " else "FAIL") name 
        (interval_to_string res) (interval_to_string expected)
    in
    check_mul "[2, 3] * [4, 5]" (abstract_range 2 3) (abstract_range 4 5) (abstract_range 8 15);
    check_mul "[-2, 3] * [-4, 5]" (abstract_range (-2) 3) (abstract_range (-4) 5) (abstract_range (-12) 15);
    check_mul "[-5, -2] * [-4, -1]" (abstract_range (-5) (-2)) (abstract_range (-4) (-1)) (abstract_range 2 20);
    check_mul "[-5, 2] * [4, 1]" (abstract_range (-5) (2)) (abstract_range (4) (1)) (abstract_range (-20) 8);
    check_mul "[0, 5] * Bottom" (abstract_range 0 5) Bottom Bottom;
    print_newline ();

    (* --- TEST DIVISIONE --- *)
    Printf.printf "===== TEST div =====\n";
    let check_div name a b expected =
      let res = div a b in
      let ok = res = expected in
      Printf.printf "[%s] div(%s) -> %s (atteso %s)\n"
        (if ok then " OK " else "FAIL") name 
        (interval_to_string res) (interval_to_string expected)
    in
    check_div "[10, 20] / [2, 5]" (abstract_range 10 20) (abstract_range 2 5) (abstract_range 2 10);
    check_div "[10, 20] / [-5, -2]" (abstract_range 10 20) (abstract_range (-5) (-2)) (abstract_range (-10) (-2));
    check_div "[10, 20] / [0, 0]" (abstract_range 10 20) (abstract_range 0 0) Bottom;
    print_newline ()
end