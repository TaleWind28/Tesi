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
    "Signs - Test Prog", List.assoc "Test Prog" Test_alcosigns.tests;

    "Sign - Filter - Certain", List.assoc "Filter - casi certi" Test_alcosigns.tests;
    "Sign - Filter - Uncertain", List.assoc "Filter - casi ambigui" Test_alcosigns.tests;
    "Sign - Filter - Simmetric", List.assoc "Filter - simmetria" Test_alcosigns.tests;
    "Sign - Filter - Derived", List.assoc "Filter - operatori derivati" Test_alcosigns.tests;
    "Sign - Filter - Composition", List.assoc "Filter - composizione And/Or/Not" Test_alcosigns.tests;
    "Sign - Filter - Bottom", List.assoc "Filter - valore Bottom" Test_alcosigns.tests;
    "Sign - Filter - Chaining", List.assoc "Filter - incatenato" Test_alcosigns.tests;
    (*Test Su IF*)
    "Sign - IF - Ramo Then", List.assoc "IF - Ramo Then" Test_alcosigns.tests;
    "Sign - IF - Ramo Else", List.assoc "IF - Ramo Else" Test_alcosigns.tests;
    "Sign - IF - Ambiguità", List.assoc "IF - Ambiguità" Test_alcosigns.tests;
    "Sign - IF - Assegnamento Parziale", List.assoc "IF - Assegnamento parziale" Test_alcosigns.tests;
    "Sign - IF - Ambiente Indipendente", List.assoc "IF - Ambiente Indipendente" Test_alcosigns.tests;
    "Sign - IF - Annidazioni", List.assoc "IF - Annidazioni" Test_alcosigns.tests;
    "Sign - IF - Condizioni Composte", List.assoc "IF - Condizioni Composte" Test_alcosigns.tests;
    "Sign - IF - Propagazione di BottomEnv", List.assoc "IF - Propagazione di BottomEnv" Test_alcosigns.tests;
    (* Test su While *)
    "Sign - While - Non Eseguito",List.assoc "While - non eseguito" Test_alcosigns.tests;
    "Sign - While - Converge",List.assoc "While - converge" Test_alcosigns.tests;
    "Sign - While - Perdita Precisione",List.assoc "While - perdita precisione" Test_alcosigns.tests;
    "Sign - While - Loop Infinito",List.assoc "While - loop infinito" Test_alcosigns.tests;
    "Sign - While - Propagazione Bottom",List.assoc "While - propagazione bottom" Test_alcosigns.tests;
    "Sign - While - Boundary Uguaglianza",List.assoc "While - boundary uguaglianza" Test_alcosigns.tests;
    "Sign - While - Annidati",List.assoc "While - annidati" Test_alcosigns.tests;
    "Sign - While - Personalizzati",List.assoc "While - Personali" Test_alcosigns.tests;
  ]
  (* @ Test_alcointervals.tests*)
  )