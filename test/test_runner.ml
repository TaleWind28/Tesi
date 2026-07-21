let () =
  Alcotest.run "Abstract Interpretation Tests" ([
    (* Espande i sottogruppi di Test_alcosigns *)
    "Signs - Somma", List.assoc "Somma" Test_alcosigns.tests;
    "Signs - Sottrazione", List.assoc "Sottrazione" Test_alcosigns.tests;
    "Signs - Moltiplicazione", List.assoc "Moltiplicazione" Test_alcosigns.tests;
    "Signs - Divisione", List.assoc "Divisione" Test_alcosigns.tests;
    "Signs - Negazione", List.assoc "Negazione" Test_alcosigns.tests;
    "Signs - Random", List.assoc "Random" Test_alcosigns.tests;
    (* Test sui comandi (eval_cmd / eval) *)
    "Signs - Assegnazioni", List.assoc "Assegnazioni" Test_alcosigns.tests;
    "Signs - Sequenze", List.assoc "Sequenze" Test_alcosigns.tests;
    "Signs - Overwrite", List.assoc "Overwrite" Test_alcosigns.tests;
    "Signs - Skip", List.assoc "Skip" Test_alcosigns.tests;
    "Signs - Stato precompilato", List.assoc "Stato precompilato" Test_alcosigns.tests;

  ] @ Test_alcointervals.tests)