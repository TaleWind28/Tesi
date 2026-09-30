(* open Syntax *)
open Parsers
open Interpreters


(* let read_file (filename:string) : string = 
  let ic = open_in filename in 
  let len = in_channel_length ic in 
  let content = really_input_string ic len in 
  close_in ic; 
  content *)

let filename = 
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else (
    prerr_endline " Uso : dune exec bin/main.exe <file.txt>";
    exit 1
  )
(* let prog_exp = "x = -1;y = nondet(1,50);z = nondet(1,50);x = x + 1;" *)
let test_prog = parse_cmd_from_file filename
let () =
  let risultato = ExtendedSignInterp.eval test_prog in
  print_string "\nExtendedSigns\n";
  ExtendedSignInterp.outputStatePrinter risultato;;

  let risultato = SimpleSignInterp.eval test_prog in
  print_string "\nSimpleSign\n";
  
  SimpleSignInterp.outputStatePrinter risultato;;
  let risultato = StrangeSignInterp.eval test_prog in
  print_string "\nStrangeSign\n";
  
  StrangeSignInterp.outputStatePrinter risultato;;
  print_string "\nReducedSign\n";
  
  let risultato = SignInterp.eval test_prog in
  SignInterp.outputStatePrinter risultato;;
  
  print_string "\nSign\n";
  let risultato = SimplifiedSignInterp.eval test_prog in
  SimplifiedSignInterp.outputStatePrinter risultato;;
  print_string "\nIntervals\n";
  let risultato = IntervalInterp.eval test_prog in
  IntervalInterp.outputStatePrinter risultato;;
  print_string "\nZones\n";
  ZoneInterp.print_result (ZoneInterp.eval test_prog);
  print_string "\nOctagones\n";
  OctagonInterp.print_result (OctagonInterp.eval test_prog)