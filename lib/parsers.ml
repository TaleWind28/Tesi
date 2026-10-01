(**
   =============================================================================
   MODULO PARSER PER IL LINGUAGGIO IMPERATIVO (TESI)
   =============================================================================
   
   Questo file implementa un analizzatore lessicale (Lexer) e un analizzatore
   sintattico a discesa ricorsiva (Recursive Descent Parser) con lookahead
   e backtracking deterministico O(1).
   
   Grammatica BNF supportata:
   -----------------------------------------------------------------------------
     c    ::= Skip 
            | ide = E 
            | C ; C 
            | if cond then C else C 
            | while cond do C 
            | cond ?
            | { C }
     cond ::= E comp E 
            | bool 
            | not cond 
            | cond and cond 
            | cond or cond
     E    ::= int 
            | Ide 
            | E bop E 
            | uop E 
            | nondet(E, E)
     comp ::= > | >= | < | <= | == | !=
     bop  ::= + | - | * | /
     uop  ::= -
     Ide  ::= string
   -----------------------------------------------------------------------------

   L'albero sintattico astratto (AST) prodotto fa direttamente riferimento ai
   tipi definiti nel modulo [Syntax]:
     - Syntax.cmd
     - Syntax.cond
     - Syntax.exp
     - Syntax.bop
     - Syntax.uop
     - Syntax.comparator
*)

(* =============================================================================
   1. DEFINIZIONE DEI TOKEN E ECCEZIONI
   ============================================================================= *)

(** Tipi di token lessicali generati dall'analizzatore. *)
type token_kind =
  (* Parole chiave per i comandi *)
  | T_SKIP
  | T_IF
  | T_THEN
  | T_ELSE
  | T_WHILE
  | T_DO
  | T_FILTER          (* Parola chiave opzionale per il comando filtro: filter(...) *)
  
  (* Parole chiave e costanti booleane *)
  | T_NOT
  | T_AND
  | T_OR
  | T_BOOL of bool    (* true | false *)

  (* Costruttori speciali di espressioni e operatori unari/postfissi *)
  | T_NONDET          (* nondet(...) oppure Random(...) *)
  | T_INC             (* inc(...) *)
  | T_DEC             (* dec(...) *)
  | T_PLUSPLUS        (* ++ *)
  | T_MINUSMINUS      (* -- *)

  (* Identificatori e costanti numeriche *)
  | T_IDE of string   (* Nomi di variabili *)
  | T_INT of int      (* Costanti intere *)

  (* Operatori aritmetici (bop e uop) *)
  | T_PLUS            (* + *)
  | T_MINUS           (* - (usato sia per bop Sub sia per uop Negation) *)
  | T_STAR            (* * *)
  | T_SLASH           (* / *)

  (* Operatori relazionali e di assegnamento *)
  | T_ASSIGN          (* = per assegnamento *)
  | T_EQ              (* == per confronto *)
  | T_NEQ             (* != o <> *)
  | T_LT              (* < *)
  | T_LTE             (* <= *)
  | T_GT              (* > *)
  | T_GTE             (* >= *)

  (* Segni di punteggiatura e delimitatori *)
  | T_QUESTION        (* ? (utilizzato nel filtro cond ?) *)
  | T_SEMI            (* ; (separatore di sequenza) *)
  | T_COMMA           (* , (separatore di argomenti, es. nondet(1, 5)) *)
  | T_LPAREN          (* ( *)
  | T_RPAREN          (* ) *)
  | T_LBRACE          (* { *)
  | T_RBRACE          (* } *)
  | T_EOF             (* Fine del flusso di input *)

(** Struttura completa del token, arricchita con riga e colonna per messaggi d'errore precisi. *)
type token = {
  kind : token_kind;
  line : int;
  col  : int;
}

(** Eccezione sollevata in caso di errore lessicale o sintattico. *)
exception ParseError of {
  msg  : string;
  line : int;
  col  : int;
}

(** Formatta un'eccezione [ParseError] in una stringa leggibile. *)
let string_of_parse_error = function
  | ParseError { msg; line; col } ->
      Printf.sprintf "Errore di parsing alla riga %d, colonna %d: %s" line col msg
  | exn -> Printexc.to_string exn

(** Rappresentazione testuale di un tipo di token (utile nei messaggi di debug ed errore). *)
let string_of_token_kind = function
  | T_SKIP -> "Skip"
  | T_IF -> "if"
  | T_THEN -> "then"
  | T_ELSE -> "else"
  | T_WHILE -> "while"
  | T_DO -> "do"
  | T_FILTER -> "filter"
  | T_NOT -> "not"
  | T_AND -> "and"
  | T_OR -> "or"
  | T_BOOL b -> string_of_bool b
  | T_NONDET -> "nondet"
  | T_INC -> "inc"
  | T_DEC -> "dec"
  | T_PLUSPLUS -> "++"
  | T_MINUSMINUS -> "--"
  | T_IDE s -> Printf.sprintf "identificatore '%s'" s
  | T_INT n -> Printf.sprintf "intero %d" n
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_STAR -> "*"
  | T_SLASH -> "/"
  | T_ASSIGN -> "="
  | T_EQ -> "=="
  | T_NEQ -> "!="
  | T_LT -> "<"
  | T_LTE -> "<="
  | T_GT -> ">"
  | T_GTE -> ">="
  | T_QUESTION -> "?"
  | T_SEMI -> ";"
  | T_COMMA -> ","
  | T_LPAREN -> "("
  | T_RPAREN -> ")"
  | T_LBRACE -> "{"
  | T_RBRACE -> "}"
  | T_EOF -> "<EOF>"

let string_of_token t = string_of_token_kind t.kind


(* =============================================================================
   2. ANALIZZATORE LESSICALE (LEXER)
   ============================================================================= *)

(**
   Il Lexer scansiona una stringa di caratteri producendo un array di token.
   Tiene traccia della posizione esatta (riga e colonna) per ogni token.
   Supporta:
   - Spazi, tabulazioni e newline.
   - Commenti a riga singola: '//' e '#'
   - Commenti a blocco annidabili: '(* ... *)' e '/* ... */'
*)
let tokenize (src : string) : token array =
  let len = String.length src in
  let pos = ref 0 in
  let line = ref 1 in
  let col = ref 1 in
  let tokens = ref [] in

  (* Funzioni ausiliarie di navigazione nel buffer di caratteri *)
  let peek () =
    if !pos < len then src.[!pos] else '\000'
  in
  let peek_next () =
    if !pos + 1 < len then src.[!pos + 1] else '\000'
  in
  let advance () =
    if !pos < len then begin
      let c = src.[!pos] in
      incr pos;
      if c = '\n' then begin
        incr line;
        col := 1
      end else
        incr col
    end
  in

  let emit kind l c =
    tokens := { kind; line = l; col = c } :: !tokens
  in

  let is_digit c = c >= '0' && c <= '9' in
  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let is_alphanumeric c = is_alpha c || is_digit c in

  (* Ciclo principale di scansione *)
  while !pos < len do
    let c = peek () in
    let cur_line = !line in
    let cur_col = !col in

    match c with
    (* Spazi bianchi *)
    | ' ' | '\t' | '\r' | '\n' ->
        advance ()

    (* Commenti a riga singola con '//' *)
    | '/' when peek_next () = '/' ->
        advance (); advance ();
        while !pos < len && peek () <> '\n' do
          advance ()
        done

    (* Commenti a riga singola con '#' *)
    | '#' ->
        advance ();
        while !pos < len && peek () <> '\n' do
          advance ()
        done

    (* Commenti a blocco in stile C '/* ... */' *)
    | '/' when peek_next () = '*' ->
        advance (); advance ();
        let closed = ref false in
        while !pos < len && not !closed do
          if peek () = '*' && peek_next () = '/' then begin
            advance (); advance ();
            closed := true
          end else
            advance ()
        done;
        if not !closed then
          raise (ParseError {
            msg = "Commento a blocco '/*' non chiuso";
            line = cur_line;
            col = cur_col;
          })

    (* Commenti a blocco in stile OCaml '(* ... *)' con supporto all'annidamento *)
    | '(' when peek_next () = '*' ->
        advance (); advance ();
        let depth = ref 1 in
        while !pos < len && !depth > 0 do
          if peek () = '(' && peek_next () = '*' then begin
            advance (); advance ();
            incr depth
          end else if peek () = '*' && peek_next () = ')' then begin
            advance (); advance ();
            decr depth
          end else
            advance ()
        done;
        if !depth > 0 then
          raise (ParseError {
            msg = "Commento a blocco '(*' non chiuso";
            line = cur_line;
            col = cur_col;
          })

    (* Operatori aritmetici e punteggiatura *)
    | '+' when peek_next () = '+' ->
        advance (); advance ();
        emit T_PLUSPLUS cur_line cur_col
    | '+' -> advance (); emit T_PLUS cur_line cur_col
    | '-' when peek_next () = '-' ->
        advance (); advance ();
        emit T_MINUSMINUS cur_line cur_col
    | '-' -> advance (); emit T_MINUS cur_line cur_col
    | '*' -> advance (); emit T_STAR cur_line cur_col
    | '/' -> advance (); emit T_SLASH cur_line cur_col
    | ';' -> advance (); emit T_SEMI cur_line cur_col
    | ',' -> advance (); emit T_COMMA cur_line cur_col
    | '?' -> advance (); emit T_QUESTION cur_line cur_col
    | '(' -> advance (); emit T_LPAREN cur_line cur_col
    | ')' -> advance (); emit T_RPAREN cur_line cur_col
    | '{' -> advance (); emit T_LBRACE cur_line cur_col
    | '}' -> advance (); emit T_RBRACE cur_line cur_col

    (* Operatore di assegnamento o uguaglianza: '=' o '==' *)
    | '=' when peek_next () = '=' ->
        advance (); advance ();
        emit T_EQ cur_line cur_col
    | '=' ->
        advance ();
        emit T_ASSIGN cur_line cur_col

    (* Disuguaglianza: '!=' o '<>' *)
    | '!' when peek_next () = '=' ->
        advance (); advance ();
        emit T_NEQ cur_line cur_col
    | '<' when peek_next () = '>' ->
        advance (); advance ();
        emit T_NEQ cur_line cur_col

    (* Minore o minore-uguale: '<' o '<=' *)
    | '<' when peek_next () = '=' ->
        advance (); advance ();
        emit T_LTE cur_line cur_col
    | '<' ->
        advance ();
        emit T_LT cur_line cur_col

    (* Maggiore o maggiore-uguale: '>' o '>=' *)
    | '>' when peek_next () = '=' ->
        advance (); advance ();
        emit T_GTE cur_line cur_col
    | '>' ->
        advance ();
        emit T_GT cur_line cur_col

    (* Costanti intere non negative *)
    | c when is_digit c ->
        let start_pos = !pos in
        while is_digit (peek ()) do
          advance ()
        done;
        let num_str = String.sub src start_pos (!pos - start_pos) in
        let n =
          try int_of_string num_str
          with _ ->
            raise (ParseError {
              msg = Printf.sprintf "Costante intera fuori scala: %s" num_str;
              line = cur_line;
              col = cur_col;
            })
        in
        emit (T_INT n) cur_line cur_col

    (* Identificatori e parole chiave *)
    | c when is_alpha c ->
        let start_pos = !pos in
        while is_alphanumeric (peek ()) do
          advance ()
        done;
        let id = String.sub src start_pos (!pos - start_pos) in
        let lower = String.lowercase_ascii id in
        let kind = match lower with
          | "skip"   -> T_SKIP
          | "if"     -> T_IF
          | "then"   -> T_THEN
          | "else"   -> T_ELSE
          | "while"  -> T_WHILE
          | "do"     -> T_DO
          | "filter" -> T_FILTER
          | "not"    -> T_NOT
          | "and"    -> T_AND
          | "or"     -> T_OR
          | "true"   -> T_BOOL true
          | "false"  -> T_BOOL false
          | "nondet" -> T_NONDET
          | "random" -> T_NONDET   (* Supporta sia la notazione BNF 'nondet' sia 'Random' *)
          | "inc"    -> T_INC
          | "dec"    -> T_DEC
          | _        -> T_IDE id
        in
        emit kind cur_line cur_col

    (* Carattere non riconosciuto *)
    | other ->
        advance ();
        raise (ParseError {
          msg = Printf.sprintf "Carattere inatteso: '%c'" other;
          line = cur_line;
          col = cur_col;
        })
  done;

  (* Aggiunta del token EOF finale *)
  tokens := { kind = T_EOF; line = !line; col = !col } :: !tokens;
  Array.of_list (List.rev !tokens)


(* =============================================================================
   3. STATO DEL PARSER E FUNZIONI AUSILIARIE DI LOOKAHEAD
   ============================================================================= *)

(**
   Stato mutabile del parser.
   L'uso di un puntatore intero [pos] su un array statico di token consente:
   - Ispezione del token corrente [peek] in O(1).
   - Avanzamento [advance] in O(1).
   - Checkpoint e ripristino di posizione (backtracking deterministico) in O(1).
*)
type parser_state = {
  tokens : token array;
  mutable pos : int;
}

let create_state (tokens : token array) : parser_state = {
  tokens;
  pos = 0;
}

let is_eof state =
  state.pos >= Array.length state.tokens ||
  state.tokens.(state.pos).kind = T_EOF

let peek state : token =
  if state.pos < Array.length state.tokens then
    state.tokens.(state.pos)
  else
    { kind = T_EOF; line = 0; col = 0 }

let peek_kind state : token_kind =
  (peek state).kind

let advance state : token =
  let t = peek state in
  if state.pos < Array.length state.tokens then
    state.pos <- state.pos + 1;
  t

(** Salva la posizione attuale per eventuale backtracking *)
let save_pos state : int = state.pos

(** Ripristina la posizione salvata *)
let restore_pos state p : unit = state.pos <- p

(** Solleva un'eccezione di parsing relativa al token corrente *)
let parse_error state msg =
  let t = peek state in
  raise (ParseError { msg; line = t.line; col = t.col })

(** Verifica e consuma un token atteso, altrimenti solleva errore *)
let expect state expected_kind =
  let t = peek state in
  if t.kind = expected_kind then
    ignore (advance state)
  else
    parse_error state (Printf.sprintf "Atteso '%s', trovato '%s'"
      (string_of_token_kind expected_kind)
      (string_of_token t))

(** Prova a eseguire una sotto-funzione di parsing; se fallisce, ripristina la posizione iniziale *)
let try_parse state (f : unit -> 'a) : 'a option =
  let saved = save_pos state in
  try
    Some (f ())
  with ParseError _ ->
    restore_pos state saved;
    None

(** Converte un token relazionale in un costruttore [Syntax.comparator] *)
let comparator_of_token = function
  | T_GT -> Some Syntax.Bigger
  | T_GTE -> Some Syntax.BiggerEquals
  | T_LT -> Some Syntax.Smaller
  | T_LTE -> Some Syntax.SmallerEquals
  | T_EQ -> Some Syntax.Equals
  | T_NEQ -> Some Syntax.NotEquals
  | _ -> None

(**
   Valutazione statica di espressioni intere costanti.
   Utilizzata per garantire che gli argomenti di [nondet(E, E)] o [Random(E, E)]
   possano essere mappati nel costruttore AST [Syntax.Random of int * int].
*)
let rec eval_const_exp = function
  | Syntax.Const n -> Some n
  | Syntax.UnaryOperation (Syntax.Negation, e) ->
      Option.map (fun n -> -n) (eval_const_exp e)
  | Syntax.BinaryOperation (e1, bop, e2) ->
      (match eval_const_exp e1, eval_const_exp e2 with
       | Some n1, Some n2 ->
           (match bop with
            | Syntax.Add -> Some (n1 + n2)
            | Syntax.Sub -> Some (n1 - n2)
            | Syntax.Mul -> Some (n1 * n2)
            | Syntax.Div -> if n2 <> 0 then Some (n1 / n2) else None)
       | _ -> None)
  | Syntax.Inc e -> Option.map (fun n -> n + 1) (eval_const_exp e)
  | Syntax.Dec e -> Option.map (fun n -> n - 1) (eval_const_exp e)
  | _ -> None


(* =============================================================================
   4. ANALISI DELLE ESPRESSIONI ARITMETICHE (E)
   ============================================================================= *)

(**
   Grammatica delle Espressioni Aritmetiche:
   -----------------------------------------------------------------------------
     E          ::= E_add
     E_add      ::= E_mul ( ('+' | '-') E_mul )*        (associativo a sinistra)
     E_mul      ::= E_unary ( ('*' | '/') E_unary )*    (associativo a sinistra)
     E_unary    ::= '-' E_unary | '+' E_unary | E_prim  (unario prefisso)
     E_prim     ::= int
                  | Ide
                  | nondet(E, E) | Random(E, E)
                  | inc(E) | dec(E)
                  | '(' E ')'
   -----------------------------------------------------------------------------
*)

let rec parse_exp state : Syntax.exp =
  parse_exp_add state

and parse_exp_add state : Syntax.exp =
  let rec loop left =
    match peek_kind state with
    | T_PLUS ->
        ignore (advance state);
        let right = parse_exp_mul state in
        let expr =
          match right with
          | Syntax.Const 1 -> Syntax.Inc left
          | _ -> Syntax.BinaryOperation (left, Syntax.Add, right)
        in
        loop expr
    | T_MINUS ->
        ignore (advance state);
        let right = parse_exp_mul state in
        let expr =
          match right with
          | Syntax.Const 1 -> Syntax.Dec left
          | _ -> Syntax.BinaryOperation (left, Syntax.Sub, right)
        in
        loop expr
    | _ -> left
  in
  let left = parse_exp_mul state in
  loop left

and parse_exp_mul state : Syntax.exp =
  let rec loop left =
    match peek_kind state with
    | T_STAR ->
        ignore (advance state);
        let right = parse_exp_unary state in
        loop (Syntax.BinaryOperation (left, Syntax.Mul, right))
    | T_SLASH ->
        ignore (advance state);
        let right = parse_exp_unary state in
        loop (Syntax.BinaryOperation (left, Syntax.Div, right))
    | _ -> left
  in
  let left = parse_exp_unary state in
  loop left

and parse_exp_unary state : Syntax.exp =
  match peek_kind state with
  | T_MINUS ->
      ignore (advance state);
      (* Ottimizzazione/adattamento: se il segno meno precede direttamente una
         costante intera positiva, la trasforma subito in Const (-n).
         Questo garantisce la massima compatibilità con i pattern matching nei domini relazionali. *)
      (match peek_kind state with
       | T_INT n ->
           ignore (advance state);
           Syntax.Const (-n)
       | _ ->
           let sub = parse_exp_unary state in
           Syntax.UnaryOperation (Syntax.Negation, sub))
  | T_MINUSMINUS ->
      ignore (advance state);
      let sub = parse_exp_unary state in
      Syntax.UnaryOperation (Syntax.Negation, Syntax.UnaryOperation (Syntax.Negation, sub))
  | T_PLUSPLUS ->
      ignore (advance state);
      let sub = parse_exp_primary state in
      Syntax.Inc sub
  | T_PLUS ->
      ignore (advance state);
      parse_exp_unary state
  | _ ->
      parse_exp_primary state

and parse_exp_primary state : Syntax.exp =
  let t = peek state in
  match t.kind with
  | T_INT n ->
      ignore (advance state);
      Syntax.Const n

  | T_IDE name ->
      ignore (advance state);
      (match peek_kind state with
       | T_PLUSPLUS ->
           ignore (advance state);
           Syntax.Inc (Syntax.Var name)
       | T_MINUSMINUS ->
           ignore (advance state);
           Syntax.Dec (Syntax.Var name)
       | _ ->
           Syntax.Var name)

  (* Scelta non deterministica: nondet(E, E) oppure Random(E, E) *)
  | T_NONDET ->
      ignore (advance state);
      expect state T_LPAREN;
      let e1 = parse_exp state in
      expect state T_COMMA;
      let e2 = parse_exp state in
      expect state T_RPAREN;
      (match eval_const_exp e1, eval_const_exp e2 with
       | Some n1, Some n2 ->
           Syntax.Random (n1, n2)
       | _ ->
           parse_error state "Gli estremi di nondet/Random devono essere costanti intere valutabili")

  (* Incremento *)
  | T_INC ->
      ignore (advance state);
      expect state T_LPAREN;
      let e = parse_exp state in
      expect state T_RPAREN;
      Syntax.Inc e

  (* Decremento *)
  | T_DEC ->
      ignore (advance state);
      expect state T_LPAREN;
      let e = parse_exp state in
      expect state T_RPAREN;
      Syntax.Dec e

  (* Espressione tra parentesi *)
  | T_LPAREN ->
      ignore (advance state);
      let e = parse_exp state in
      expect state T_RPAREN;
      e

  | _ ->
      parse_error state (Printf.sprintf "Attesa espressione aritmetica, trovato '%s'"
        (string_of_token t))


(* =============================================================================
   5. ANALISI DELLE CONDIZIONI BOOLEANE (cond)
   ============================================================================= *)

(**
   Grammatica delle Condizioni:
   -----------------------------------------------------------------------------
     cond       ::= cond_or
     cond_or    ::= cond_and ( 'or' cond_and )*      (associativo a sinistra)
     cond_and   ::= cond_not ( 'and' cond_not )*     (associativo a sinistra)
     cond_not   ::= 'not' cond_not | cond_atom       (unario prefisso)
     cond_atom  ::= bool
                  | '(' cond ')'
                  | E comp E
   -----------------------------------------------------------------------------
*)

let rec parse_cond state : Syntax.cond =
  parse_cond_or state

and parse_cond_or state : Syntax.cond =
  let rec loop left =
    match peek_kind state with
    | T_OR ->
        ignore (advance state);
        let right = parse_cond_and state in
        loop (Syntax.Or (left, right))
    | _ -> left
  in
  let left = parse_cond_and state in
  loop left

and parse_cond_and state : Syntax.cond =
  let rec loop left =
    match peek_kind state with
    | T_AND ->
        ignore (advance state);
        let right = parse_cond_not state in
        loop (Syntax.And (left, right))
    | _ -> left
  in
  let left = parse_cond_not state in
  loop left

and parse_cond_not state : Syntax.cond =
  match peek_kind state with
  | T_NOT ->
      ignore (advance state);
      let sub = parse_cond_not state in
      Syntax.Not sub
  | _ ->
      parse_cond_atom state

and parse_cond_atom state : Syntax.cond =
  match peek_kind state with
  | T_BOOL b ->
      ignore (advance state);
      Syntax.Boolean b

  | T_LPAREN ->
      (*
         Può trattarsi di:
         1. Una condizione racchiusa tra parentesi, es: (x > 0 and y < 5)
         2. Un'espressione a sinistra di un confronto, es: (x + 1) > 2
      *)
      let saved = save_pos state in
      ignore (advance state);
      (match try_parse state (fun () ->
         let c = parse_cond_or state in
         expect state T_RPAREN;
         c
       ) with
       | Some c ->
           (* Se dopo la parentesi chiusa c'è un operatore di confronto, significa
              che le parentesi racchiudevano solo l'espressione di sinistra! *)
           (match comparator_of_token (peek_kind state) with
            | Some _ ->
                restore_pos state saved;
                parse_comparison state
            | None -> c)
       | None ->
           restore_pos state saved;
           parse_comparison state)

  | _ ->
      parse_comparison state

and parse_comparison state : Syntax.cond =
  let e1 = parse_exp state in
  match comparator_of_token (peek_kind state) with
  | Some cmp ->
      ignore (advance state);
      let e2 = parse_exp state in
      Syntax.Comparison (e1, cmp, e2)
  | None ->
      parse_error state (Printf.sprintf "Atteso operatore di confronto (>, >=, <, <=, ==, !=), trovato '%s'"
        (string_of_token (peek state)))


(* =============================================================================
   6. ANALISI DEI COMANDI (c)
   ============================================================================= *)

(**
   Grammatica dei Comandi:
   -----------------------------------------------------------------------------
     c          ::= c_seq
     c_seq      ::= c_atom ( ';' c_atom? )*
     c_atom     ::= Skip
                  | 'if' cond 'then' c_atom 'else' c_atom
                  | 'while' cond 'do' c_atom
                  | 'filter' '('? cond ')'?
                  | cond '?'
                  | Ide '=' E
                  | '{' c_seq '}'
   -----------------------------------------------------------------------------
*)

let rec parse_cmd state : Syntax.cmd =
  parse_cmd_seq state

and parse_cmd_seq state : Syntax.cmd =
  (* Consuma eventuali punti e virgola ridondanti iniziali *)
  while peek_kind state = T_SEMI do
    ignore (advance state)
  done;

  if is_eof state ||
     peek_kind state = T_RPAREN ||
     peek_kind state = T_RBRACE then
    Syntax.Skip
  else
    let first = parse_cmd_atom state in
    if peek_kind state = T_SEMI then begin
      ignore (advance state);
      (* Se dopo il ';' c'è EOF o chiusura di blocco, la sequenza termina *)
      if is_eof state ||
         peek_kind state = T_RPAREN ||
         peek_kind state = T_RBRACE then
        first
      else
        let rest = parse_cmd_seq state in
        Syntax.Sequence (first, rest)
    end else
      first

and parse_cmd_atom state : Syntax.cmd =
  let t = peek state in
  match t.kind with
  (* 1. Comando Skip *)
  | T_SKIP ->
      ignore (advance state);
      Syntax.Skip

  (* 2. Condizionale: if cond then C else C *)
  | T_IF ->
      ignore (advance state);
      let condition = parse_cond state in
      expect state T_THEN;
      let then_branch = parse_cmd_atom state in
      expect state T_ELSE;
      let else_branch = parse_cmd_atom state in
      Syntax.If (condition, then_branch, else_branch)

  (* 3. Ciclo: while cond do C *)
  | T_WHILE ->
      ignore (advance state);
      let condition = parse_cond state in
      expect state T_DO;
      let body = parse_cmd_atom state in
      Syntax.While (condition, body)

  (* 4. Filtro con parola chiave esplicita: filter(cond) o filter cond *)
  | T_FILTER ->
      ignore (advance state);
      let has_paren = (peek_kind state = T_LPAREN) in
      if has_paren then ignore (advance state);
      let condition = parse_cond state in
      if has_paren then expect state T_RPAREN;
      if peek_kind state = T_QUESTION then ignore (advance state);
      Syntax.Filter condition

  (* 5. Blocchi delimitati da parentesi graffe { ... } *)
  | T_LBRACE ->
      ignore (advance state);
      let cmd = parse_cmd_seq state in
      expect state T_RBRACE;
      cmd

  (* 6. Comandi prefissi di incremento e decremento: ++x oppure --x *)
  | T_PLUSPLUS ->
      ignore (advance state);
      let ide_name =
        match peek_kind state with
        | T_IDE name -> ignore (advance state); name
        | _ -> parse_error state (Printf.sprintf "Atteso identificatore dopo '++', trovato '%s'" (string_of_token (peek state)))
      in
      Syntax.Assign (ide_name, Syntax.Inc (Syntax.Var ide_name))

  | T_MINUSMINUS ->
      ignore (advance state);
      let ide_name =
        match peek_kind state with
        | T_IDE name -> ignore (advance state); name
        | _ -> parse_error state (Printf.sprintf "Atteso identificatore dopo '--', trovato '%s'" (string_of_token (peek state)))
      in
      Syntax.Assign (ide_name, Syntax.Dec (Syntax.Var ide_name))

  (* 7. Gestione di filtri cond ? e assegnamenti *)
  | _ ->
      (* Tentativo A: verificare se il comando corrente è un filtro `cond ?` *)
      let saved = save_pos state in
      let filter_opt =
        try_parse state (fun () ->
          let c = parse_cond state in
          if peek_kind state = T_QUESTION then begin
            ignore (advance state);
            Some (Syntax.Filter c)
          end else
            None
        )
      in
      (match filter_opt with
       | Some (Some filter_cmd) -> filter_cmd
       | _ ->
           restore_pos state saved;

           (* Tentativo B: blocco o comando racchiuso tra parentesi tonde ( C ) *)
           if peek_kind state = T_LPAREN then begin
             ignore (advance state);
             let cmd = parse_cmd_seq state in
             expect state T_RPAREN;
             cmd
           end
           (* Assegnamento ide = E oppure incremento/decremento ide++ / ide-- *)
           else if (match peek_kind state with T_IDE _ -> true | _ -> false) then begin
             let ide_name =
               match advance state with
               | { kind = T_IDE name; _ } -> name
               | _ -> assert false
             in
             match peek_kind state with
             | T_PLUSPLUS ->
                 ignore (advance state);
                 Syntax.Assign (ide_name, Syntax.Inc (Syntax.Var ide_name))
             | T_MINUSMINUS ->
                 ignore (advance state);
                 Syntax.Assign (ide_name, Syntax.Dec (Syntax.Var ide_name))
             | T_ASSIGN ->
                 ignore (advance state);
                 let expr = parse_exp state in
                 Syntax.Assign (ide_name, expr)
             | _ ->
                 expect state T_ASSIGN;
                 assert false
           end
           else
             parse_error state (Printf.sprintf "Comando inatteso o non valido: '%s'"
               (string_of_token (peek state))))


(* =============================================================================
   7. INTERFACCIA PUBBLICA E FUNZIONI DI UTILITÀ
   ============================================================================= *)

(**
   Esegue il parsing di una stringa contenente un comando o un intero programma.
   Esempio:
     [Parser.parse_cmd "x = Random(1, 5); y = -x + 10; x <= 5 ?"]
*)
let parse_cmd_from_string (input : string) : Syntax.cmd =
  let tokens = tokenize input in
  let state = create_state tokens in
  let result = parse_cmd state in
  if not (is_eof state) then
    parse_error state (Printf.sprintf "Token residui inattesi al termine del programma: '%s'"
      (string_of_token (peek state)));
  result

(** Alias conciso per [parse_cmd_from_string] *)
let parse_cmd = parse_cmd_from_string

(**
   Esegue il parsing di una stringa contenente una sola espressione aritmetica.
   Esempio:
     [Parser.parse_exp "x + 2 * y - nondet(1, 5)"]
*)
let parse_exp_from_string (input : string) : Syntax.exp =
  let tokens = tokenize input in
  let state = create_state tokens in
  let result = parse_exp state in
  if not (is_eof state) then
    parse_error state (Printf.sprintf "Token residui inattesi al termine dell'espressione: '%s'"
      (string_of_token (peek state)));
  result

(** Alias conciso per [parse_exp_from_string] *)
let parse_exp = parse_exp_from_string

(**
   Esegue il parsing di una stringa contenente una sola condizione booleana.
   Esempio:
     [Parser.parse_cond "x > 0 and (y <= 10 or not false)"]
*)
let parse_cond_from_string (input : string) : Syntax.cond =
  let tokens = tokenize input in
  let state = create_state tokens in
  let result = parse_cond state in
  if not (is_eof state) then
    parse_error state (Printf.sprintf "Token residui inattesi al termine della condizione: '%s'"
      (string_of_token (peek state)));
  result

(** Alias conciso per [parse_cond_from_string] *)
let parse_cond = parse_cond_from_string

(**
   Esegue il parsing di un file sorgente letto direttamente dal disco.
   Esempio:
     [Parser.parse_cmd_from_file "programma.txt"]
*)
let parse_cmd_from_file (filename : string) : Syntax.cmd =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  parse_cmd_from_string content

(**
   Esegue il parsing di un flusso di input aperto [in_channel].
*)
let parse_cmd_from_channel (ic : in_channel) : Syntax.cmd =
  let buf = Buffer.create 1024 in
  (try
     while true do
       let line = input_line ic in
       Buffer.add_string buf line;
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  parse_cmd_from_string (Buffer.contents buf)


(* =============================================================================
   8. PRETTY-PRINTER PER DEBUG E VISUALIZZAZIONE
   ============================================================================= *)

(** Converte un operatore binario in stringa *)
let string_of_bop = function
  | Syntax.Add -> "+"
  | Syntax.Sub -> "-"
  | Syntax.Mul -> "*"
  | Syntax.Div -> "/"

(** Converte un operatore unario in stringa *)
let string_of_uop = function
  | Syntax.Negation -> "-"

(** Converte un comparatore in stringa *)
let string_of_comparator = function
  | Syntax.Equals -> "=="
  | Syntax.Bigger -> ">"
  | Syntax.Smaller -> "<"
  | Syntax.BiggerEquals -> ">="
  | Syntax.SmallerEquals -> "<="
  | Syntax.NotEquals -> "!="

(** Converte un'espressione AST [Syntax.exp] in formato testuale leggibile *)
let rec string_of_exp = function
  | Syntax.Const n -> string_of_int n
  | Syntax.Var x -> x
  | Syntax.BinaryOperation (e1, bop, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_exp e1) (string_of_bop bop) (string_of_exp e2)
  | Syntax.UnaryOperation (uop, e) ->
      Printf.sprintf "%s(%s)" (string_of_uop uop) (string_of_exp e)
  | Syntax.Random (min_val, max_val) ->
      Printf.sprintf "nondet(%d, %d)" min_val max_val
  | Syntax.Inc e -> Printf.sprintf "inc(%s)" (string_of_exp e)
  | Syntax.Dec e -> Printf.sprintf "dec(%s)" (string_of_exp e)

(** Converte una condizione AST [Syntax.cond] in formato testuale leggibile *)
let rec string_of_cond = function
  | Syntax.Boolean b -> string_of_bool b
  | Syntax.Comparison (e1, cmp, e2) ->
      Printf.sprintf "%s %s %s" (string_of_exp e1) (string_of_comparator cmp) (string_of_exp e2)
  | Syntax.Not c -> Printf.sprintf "not (%s)" (string_of_cond c)
  | Syntax.And (c1, c2) -> Printf.sprintf "(%s and %s)" (string_of_cond c1) (string_of_cond c2)
  | Syntax.Or (c1, c2) -> Printf.sprintf "(%s or %s)" (string_of_cond c1) (string_of_cond c2)

(** Converte un comando AST [Syntax.cmd] in formato testuale leggibile indentato *)
let rec string_of_cmd_indent (indent : int) =
  let pad = String.make indent ' ' in
  function
  | Syntax.Skip -> pad ^ "Skip"
  | Syntax.Assign (x, e) ->
      pad ^ Printf.sprintf "%s = %s" x (string_of_exp e)
  | Syntax.Filter c ->
      pad ^ Printf.sprintf "%s ?" (string_of_cond c)
  | Syntax.Sequence (c1, c2) ->
      let s1 = string_of_cmd_indent indent c1 in
      let s2 = string_of_cmd_indent indent c2 in
      pad ^ String.trim s1 ^ ";\n" ^ s2
  | Syntax.If (cond, c1, c2) ->
      let s_then = string_of_cmd_indent (indent + 2) c1 in
      let s_else = string_of_cmd_indent (indent + 2) c2 in
      pad ^ Printf.sprintf "if %s then\n%s\n%selse\n%s"
        (string_of_cond cond) s_then pad s_else
  | Syntax.While (cond, body) ->
      let s_body = string_of_cmd_indent (indent + 2) body in
      pad ^ Printf.sprintf "while %s do\n%s"
        (string_of_cond cond) s_body

let string_of_cmd (c : Syntax.cmd) : string =
  string_of_cmd_indent 0 c
