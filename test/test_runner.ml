(*
let () = 
  Test_intervals.TestIntervals.run_all_tests ();
  Test_signs.TestSigns.run_all_tests ()
*)

let () =
  Alcotest.run "Abstract Interpretation Tests" ([
    (* Espande i sottogruppi di Test_alcosigns *)
    "Signs - Somma", List.assoc "Somma" Test_alcosigns.tests;
    "Signs - Sottrazione", List.assoc "Sottrazione" Test_alcosigns.tests;
    "Signs - Moltiplicazione", List.assoc "Moltiplicazione" Test_alcosigns.tests;
    "Signs - Divisione", List.assoc "Divisione" Test_alcosigns.tests;
    "Signs - Negazione", List.assoc "Negazione" Test_alcosigns.tests;
    "Signs - Random", List.assoc "Random" Test_alcosigns.tests;
  ]  @ Test_alcointervals.tests )