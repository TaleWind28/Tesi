(*value *)
module type EXPECTED_VALUES = sig
  type t

  (* Somma *)
  val sum_1 : t
  val sum_2 : t
  val sum_3 : t
  val sum_4 : t
  val sum_5 : t
  val sum_6 : t
  val sum_7 : t
  val sum_8 : t
  val sum_9 : t
  val sum_10 : t
  val sum_11 : t
  val sum_12 : t
  val sum_13 : t
  val sum_14 : t
  val sum_15 : t
  val sum_16 : t
  val sum_17 : t
  val sum_18 : t

  (* Sottrazione *)
  val sub_1 : t
  val sub_2 : t
  val sub_3 : t
  val sub_4 : t
  val sub_5 : t

  (* Moltiplicazione *)
  val mul_1 : t
  val mul_2 : t
  val mul_3 : t
  val mul_4 : t
  val mul_5 : t
  val mul_6 : t
  val mul_7 : t
  val mul_8 : t
  val mul_9 : t
  val mul_10 : t

  (* Divisione *)
  val div_1 : t
  val div_2 : t
  val div_3 : t
  val div_4 : t
  val div_5 : t
  val div_6 : t
  val div_7 : t
  val div_8 : t
  val div_9 : t
  val div_10 : t
  val div_11 : t
  val div_12 : t

  (* Negazione Unaria *)
  val neg_1 : t
  val neg_2 : t
  val neg_3 : t
  val neg_4 : t
  val neg_5 : t
  val neg_6 : t
  val neg_7 : t
  val neg_8 : t
  val neg_9 : t
  val neg_10 : t

  (* Random *)
  val rand_1 : t
  val rand_2 : t
  val rand_3 : t
  val rand_4 : t
  val rand_5 : t
  val rand_6 : t

  (* Assegnamenti *)
  val assign_1 : t
  val assign_2 : t
  val assign_3 : t
  val assign_4 : t
  val assign_5 : t

  (* Sequenze *)
  val sequence_1_1 : t
  val sequence_1_2 : t

  (* Skip *)
  val skip_1 : t

  (* If *)
  val if_1 : t
  val if_2 : t
  val if_3 : t
  val if_4 : t
  val if_5 : t
  val if_6 : t
  val if_7 : t
  val if_8 : t
  val if_9 : t
  val if_10 : t
  val if_11 : t
  val if_12 : t

  (* While *)
  val while_1_1 : t
  val while_1_2 : t
  val while_2_1 : t
  val while_3_1 : t
  val while_3_2 : t
  val while_4_1 : t
  val while_4_2 : t
  val while_4_3 : t
end
(* Aggiungi qui gli altri valori se variano tra domini *)

module Expected_Signs : EXPECTED_VALUES with type t = Abstract_domains.Signs.t  = struct
  open Abstract_domains.Signs

  type t = Abstract_domains.Signs.t

  (* Somma *)
  let sum_1 = Pos
  let sum_2 = top
  let sum_3 = Neg
  let sum_4 = Pos
  let sum_5 = Zero
  let sum_6 = PosZero
  let sum_7 = NegZero
  let sum_8 = top
  let sum_9 = Pos
  let sum_10 = top
  let sum_11 = top
  let sum_12 = Neg
  let sum_13 = top
  let sum_14 = NonZero
  let sum_15 = top
  let sum_16 = top
  let sum_17 = SignBottom
  let sum_18 = top

  (* Sottrazione *)
  let sub_1 = Pos
  let sub_2 = top
  let sub_3 = top
  let sub_4 = top
  let sub_5 = Pos

  (* Moltiplicazione *)
  let mul_1 = Pos
  let mul_2 = Neg
  let mul_3 = Pos
  let mul_4 = Zero
  let mul_5 = NegZero
  let mul_6 = NegZero
  let mul_7 = Zero
  let mul_8 = NonZero
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = PosZero
  let div_2 = NegZero
  let div_3 = PosZero
  let div_4 = SignBottom
  let div_5 = top
  let div_6 = top
  let div_7 = top
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = top
  let div_11 = NegZero
  let div_12 = NegZero

  (* Negazione Unaria *)
  let neg_1 = Neg
  let neg_2 = Pos
  let neg_3 = Zero
  let neg_4 = NegZero
  let neg_5 = PosZero
  let neg_6 = NonZero
  let neg_7 = top
  let neg_8 = SignBottom
  let neg_9 = Pos
  let neg_10 = Pos

  (* Random *)
  let rand_1 = top
  let rand_2 = Pos
  let rand_3 = Neg
  let rand_4 = PosZero
  let rand_5 = NegZero
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = Pos
  let assign_2 = Neg
  let assign_3 = Zero
  let assign_4 = top
  let assign_5 = Pos

  (* Sequenze *)
  let sequence_1_1 = Pos
  let sequence_1_2 = Neg

  (* Skip *)
  let skip_1 = Pos

  (* If *)
  let if_1 = Pos
  let if_2 = Pos
  let if_3 = Pos
  let if_4 = NonZero
  let if_5 = PosZero
  let if_6 = NegZero
  let if_7 = top
  let if_8 = Pos
  let if_9 = Pos
  let if_10 = Neg
  let if_11 = NonZero
  let if_12 = NonZero

  (* While *)
  let while_1_1 = Pos
  let while_1_2 = Neg
  let while_2_1 = Zero
  let while_3_1 = PosZero
  let while_3_2 = PosZero 
  let while_4_1 = top
  let while_4_2 = Pos
  let while_4_3 = top
end

module Expected_SimpleSigns : EXPECTED_VALUES with type t = Abstract_domains.SimpleSigns.t = struct
  open Abstract_domains.SimpleSigns
  
  type t = Abstract_domains.SimpleSigns.t
  let sum_1 = PosZero
  let sum_2 = top
  let sum_3 = NegZero
  let sum_4 = PosZero
  let sum_5 = Zero
  let sum_6 = PosZero
  let sum_7 = NegZero
  let sum_8 = top
  let sum_9 = PosZero
  let sum_10 = top
  let sum_11 = top
  let sum_12 = NegZero
  let sum_13 = top
  let sum_14 = top
  let sum_15 = top
  let sum_16 = top
  let sum_17 = SignBottom
  let sum_18 = top

  (* Sottrazione *)
  let sub_1 = PosZero
  let sub_2 = top
  let sub_3 = top
  let sub_4 = top
  let sub_5 = PosZero

  (* Moltiplicazione *)
  let mul_1 = PosZero
  let mul_2 = NegZero
  let mul_3 = PosZero
  let mul_4 = Zero
  let mul_5 = NegZero
  let mul_6 = NegZero
  let mul_7 = Zero
  let mul_8 = top
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = PosZero
  let div_2 = NegZero
  let div_3 = PosZero
  let div_4 = SignBottom
  let div_5 = PosZero
  let div_6 = NegZero
  let div_7 = top
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = top
  let div_11 = NegZero
  let div_12 = NegZero

  (* Negazione Unaria *)
  let neg_1 = NegZero
  let neg_2 = PosZero
  let neg_3 = Zero
  let neg_4 = NegZero
  let neg_5 = PosZero
  let neg_6 = top
  let neg_7 = top
  let neg_8 = SignBottom
  let neg_9 = PosZero
  let neg_10 = PosZero

  (* Random *)
  let rand_1 = top
  let rand_2 = PosZero
  let rand_3 = NegZero
  let rand_4 = PosZero
  let rand_5 = NegZero
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = PosZero
  let assign_2 = NegZero
  let assign_3 = Zero
  let assign_4 = top
  let assign_5 = PosZero

  (* Sequenze *)
  let sequence_1_1 = PosZero
  let sequence_1_2 = NegZero

  (* Skip *)
  let skip_1 = PosZero

  (* If *)
  let if_1 = top (*è corretto top*)
  let if_2 = PosZero
  let if_3 = PosZero
  let if_4 = top
  let if_5 = PosZero
  let if_6 = NegZero
  let if_7 = top
  let if_8 = PosZero
  let if_9 = PosZero
  let if_10 = NegZero
  let if_11 = top
  let if_12 = top

  (* While *)
  let while_1_1 = PosZero (*risultato corretto: PosZero*)
  let while_1_2 = NegZero (* non serve questo valore*)
  let while_2_1 = Zero (* è accettabile anche PosZero, ma ottengo bottomEnv*) 
  let while_3_1 = PosZero (*0*)
  let while_3_2 = Zero 
  let while_4_1 = top
  let while_4_2 = PosZero 
  let while_4_3 = top
end

module Expected_StrangeSigns : EXPECTED_VALUES with type t = Abstract_domains.StrangeSigns.t = struct
  open Abstract_domains.StrangeSigns

  type t = Abstract_domains.StrangeSigns.t

  (* Stato test: x=PosZero(1..10) y=Neg(-10..-1) z=Zero w=PosZero(0..10)
     k=Top(-10..0, unisce Neg e Zero) n=Top(unisce PosZero e Neg) t=Top b=Bottom *)

  (* Somma *)
  let sum_1 = PosZero
  let sum_2 = top            (* PosZero+Neg: nessun elemento copre "<=0" *)
  let sum_3 = Neg
  let sum_4 = PosZero
  let sum_5 = Zero
  let sum_6 = PosZero
  let sum_7 = top            (* k+k = Top+Top *)
  let sum_8 = top
  let sum_9 = PosZero
  let sum_10 = top
  let sum_11 = top
  let sum_12 = top
  let sum_13 = top
  let sum_14 = top
  let sum_15 = top
  let sum_16 = top
  let sum_17 = SignBottom
  let sum_18 = top           (* 10:PosZero, -20:Neg -> Top *)

  (* Sottrazione: a-b = a + Negation(b) *)
  let sub_1 = PosZero            (* PosZero + Negation(Neg)=PosZero => PosZero *)
  let sub_2 = top            (* PosZero + Negation(PosZero)=Top *)
  let sub_3 = top
  let sub_4 = top
  let sub_5 = PosZero            (* Zero + Negation(Neg)=PosZero *)

  (* Moltiplicazione *)
  let mul_1 = PosZero
  let mul_2 = top            (* PosZero*Neg: nessun "non-positivo" *)
  let mul_3 = PosZero            (* Neg*Neg: prodotto strett. positivo -> PosZero *)
  let mul_4 = Zero
  let mul_5 = top            (* PosZero*Neg *)
  let mul_6 = top            (* k=Top, non è Zero esatto: Top*PosZero *)
  let mul_7 = Zero               (* z=Zero esatto -> risultato Zero comunque *)
  let mul_8 = top
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = PosZero
  let div_2 = top            (* PosZero/Neg *)
  let div_3 = PosZero
  let div_4 = SignBottom
  let div_5 = PosZero
  let div_6 = top            (* x/k = PosZero/Top *)
  let div_7 = top
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = top
  let div_11 = top           (* w/y = PosZero/Neg *)
  let div_12 = top           (* k/x = Top/PosZero *)

  (* Negazione unaria *)
  let neg_1 = top            (* Negation(PosZero): nessun "<=0" esatto *)
  let neg_2 = PosZero            (* Negation(Neg) = PosZero *)
  let neg_3 = Zero
  let neg_4 = top            (* Negation(PosZero) *)
  let neg_5 = top            (* Negation(Top) *)
  let neg_6 = top
  let neg_7 = top
  let neg_8 = SignBottom
  let neg_9 = top            (* Negation(Negation(PosZero)) = Negation(Top) *)
  let neg_10 = PosZero           (* x + Negation(y) = PosZero+PosZero *)

  (* Random *)
  let rand_1 = top           (* attraversa Neg e PosZero *)
  let rand_2 = PosZero
  let rand_3 = Neg
  let rand_4 = PosZero
  let rand_5 = top           (* attraversa Neg e Zero *)
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = PosZero
  let assign_2 = Neg
  let assign_3 = Zero
  let assign_4 = top         (* variabile non definita -> Top *)
  let assign_5 = PosZero

  (* Sequenze *)
  let sequence_1_1 = PosZero
  let sequence_1_2 = Neg

  (* Skip *)
  let skip_1 = PosZero

  (* If *)
  let if_1 = top             (* join(PosZero,Neg), corretto che sia top *)
  let if_2 = PosZero
  let if_3 = PosZero
  let if_4 = top
  let if_5 = PosZero
  let if_6 = top             (* join(Neg,Zero): qui differisce da SimpleSigns (NegZero) *)
  let if_7 = top
  let if_8 = PosZero
  let if_9 = PosZero
  let if_10 = Neg
  let if_11 = top
  let if_12 = top

  (* While *)
  let while_1_1 = PosZero
  let while_1_2 = Neg            (* non serve questo valore *)
  let while_2_1 = Zero
  let while_3_1 = PosZero        (* vero risultato 0, perso per precisione *)
  let while_3_2 = PosZero        (* vero risultato 0, perso per precisione *)

  (* let while_3_2 = PosZero *)
  let while_4_1 = top
  let while_4_2 = PosZero        (* vedi nota sotto *)
  let while_4_3 = top
end

module Expected_SimplifiedSigns :EXPECTED_VALUES with type t = Abstract_domains.SimplifiedSigns.t = struct
  open Abstract_domains.SimplifiedSigns
  
  type t = Abstract_domains.SimplifiedSigns.t

  (* Somma *)
  let sum_1 = Pos
  let sum_2 = top
  let sum_3 = Neg
  let sum_4 = Pos
  let sum_5 = Zero
  let sum_6 = top
  let sum_7 = top
  let sum_8 = top
  let sum_9 = top
  let sum_10 = top
  let sum_11 = top
  let sum_12 = top
  let sum_13 = top
  let sum_14 = top
  let sum_15 = top
  let sum_16 = top
  let sum_17 = SignBottom
  let sum_18 = top

  (* Sottrazione *)
  let sub_1 = Pos
  let sub_2 = top
  let sub_3 = top
  let sub_4 = top
  let sub_5 = Pos

  (* Moltiplicazione *)
  let mul_1 = Pos
  let mul_2 = Neg
  let mul_3 = Pos
  let mul_4 = Zero
  let mul_5 = top
  let mul_6 = top
  let mul_7 = Zero
  let mul_8 = top
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = Pos
  let div_2 = Neg
  let div_3 = Pos
  let div_4 = bottom
  let div_5 = top
  let div_6 = top
  let div_7 = top
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = top
  let div_11 = top
  let div_12 = top

  (* Negazione Unaria *)
  let neg_1 = Neg
  let neg_2 = Pos
  let neg_3 = Zero
  let neg_4 = top
  let neg_5 = top
  let neg_6 = top
  let neg_7 = top
  let neg_8 = SignBottom
  let neg_9 = Pos
  let neg_10 = Pos

  (* Random *)
  let rand_1 = top
  let rand_2 = Pos
  let rand_3 = Neg
  let rand_4 = top
  let rand_5 = top
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = Pos
  let assign_2 = Neg
  let assign_3 = Zero
  let assign_4 = top
  let assign_5 = Pos

  (* Sequenze *)
  let sequence_1_1 = Pos
  let sequence_1_2 = Neg

  (* Skip *)
  let skip_1 = Pos

  (* If *)
  let if_1 = Pos
  let if_2 = Pos
  let if_3 = Pos
  let if_4 = top
  let if_5 = top
  let if_6 = top
  let if_7 = top
  let if_8 = Pos
  let if_9 = Pos
  let if_10 = Neg
  let if_11 = top
  let if_12 = top

  (* While *)
  let while_1_1 = Pos
  let while_1_2 = Neg
  let while_2_1 = Zero
  let while_3_1 = top

  let while_3_2 = top
  let while_4_1 = top
  let while_4_2 = Pos
  let while_4_3 = top
end

module Expected_ReducedSigns : EXPECTED_VALUES with type t = Abstract_domains.ReducedSigns.t = struct
  open Abstract_domains.ReducedSigns

  type t = Abstract_domains.ReducedSigns.t

  (* Stato: x=Pos(1..10) y=Neg(-10..-1) z=Pos(convenzione: 0->Pos)
     w=Pos(0..10) k=Neg(-10..0) n=Top(join Pos,Neg) t=Top b=Bottom *)

  (* Somma *)
  let sum_1 = Pos
  let sum_2 = top
  let sum_3 = Neg
  let sum_4 = top
  let sum_5 = top              (* ⚠️ era Zero esatto; ora Pos (0+0=0∈Pos, sound ma impreciso) *)
  let sum_6 = top
  let sum_7 = top
  let sum_8 = top
  let sum_9 = top
  let sum_10 = top
  let sum_11 = top
  let sum_12 = top
  let sum_13 = top
  let sum_14 = top
  let sum_15 = top
  let sum_16 = top
  let sum_17 = SignBottom
  let sum_18 = top

  (* Sottrazione (negazione qui è ESATTA: Pos<->Neg) *)
  let sub_1 = Pos
  let sub_2 = top
  let sub_3 = top
  let sub_4 = top
  let sub_5 = top

  (* Moltiplicazione *)
  let mul_1 = Pos
  let mul_2 = Neg
  let mul_3 = Pos              (* Neg*Neg = nonpos*nonpos ≥0 -> Pos *)
  let mul_4 = top              (* ⚠️ era Zero esatto; ora Pos*Pos=Pos *)
  let mul_5 = top
  let mul_6 = top
  let mul_7 = top          (* ⚠️ era Zero esatto (n*z, "anything*0=0"); qui n=Top perde il caso speciale, Top*Pos=Top *)
  let mul_8 = top
  let mul_9 = top          (* ⚠️ era Zero esatto (t*z); ora Top*Pos=Top *)
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = Pos
  let div_2 = Neg
  let div_3 = Pos
  let div_4 = top              (* ⚠️⚠️ era SignBottom (divisore esattamente 0); qui z=Pos non è riconoscibile come "0 esatto", quindi niente check div-by-zero: risultato Pos/Pos=Pos. ReducedSigns NON rileva più la divisione per zero certa. *)
  let div_5 = top
  let div_6 = top
  let div_7 = top
  let div_8 = top              (* ⚠️ era Zero esatto (z/x); ora Pos/Pos=Pos *)
  let div_9 = top             (* ⚠️ era Zero esatto (z/y); ora Pos/Neg=Neg *)
  let div_10 = top
  let div_11 = top
  let div_12 = top

  (* Negazione unaria — ESATTA in questo dominio *)
  let neg_1 = Neg
  let neg_2 = Pos
  let neg_3 = top              (* ⚠️ era Zero esatto; ora Negate(Pos)=Neg (conseguenza della convenzione su z, non della negazione) *)
  let neg_4 = top
  let neg_5 = top
  let neg_6 = top
  let neg_7 = top
  let neg_8 = SignBottom
  let neg_9 = Pos
  let neg_10 = Pos

  (* Random *)
  let rand_1 = top
  let rand_2 = Pos
  let rand_3 = Neg
  let rand_4 = top
  let rand_5 = top
  let rand_6 = top             (* ⚠️ era Zero esatto (Random(0,0)); convenzione -> Pos *)

  (* Assegnamenti *)
  let assign_1 = Pos
  let assign_2 = Neg
  let assign_3 = top           (* ⚠️ era Zero esatto (x=Const 0); convenzione -> Pos *)
  let assign_4 = top
  let assign_5 = Pos

  (* Sequenze *)
  let sequence_1_1 = Pos
  let sequence_1_2 = Neg

  (* Skip *)
  let skip_1 = Pos

  (* If *)
  let if_1 = top
  let if_2 = Pos
  let if_3 = Pos
  let if_4 = top
  let if_5 = top
  let if_6 = top           (* ⚠️ era NegZero; else=Const 0 ora è Pos (convenzione) non più subsumed in Neg, quindi join(Neg,Pos)=Top *)
  let if_7 = top
  let if_8 = Pos
  let if_9 = Pos
  let if_10 = Neg
  let if_11 = top
  let if_12 = top

  (* While *)
  let while_1_1 = top
  let while_1_2 = Neg
  let while_2_1 = top          (* qui NON è ambiguo Zero-vs-PosZero: Pos è l'UNICA rappresentazione più precisa disponibile per {0}, essendo Zero assente dal dominio *)
  let while_3_1 = top
  let while_3_2 = Pos
  let while_4_1 = top
  let while_4_2 = Pos          (* ⚠️ stessa ambiguità di iterazione già segnalata per StrangeSigns/SimpleSigns: potrebbe diventare top a seconda della profondità del fixpoint *)
  let while_4_3 = top
end
(* =====================================================================
   Expected_Intervals: valori attesi (corretti) per il dominio Intervals
   =====================================================================

   IMPORTANTE - tre cose da sistemare prima che questi valori combacino
   con l'output reale del tuo interprete:

   1) I test iftests/whiletests da if_4 in poi (e if_2) fanno riferimento
      a variabili "w", "x", "y", "k" gia' presenti nello stato precompilato
      make_test_state(). Ma "make_prog_case" chiama Interp.eval, che parte
      da un ambiente VUOTO (vedi "let eval prog = eval_cmd prog
      (Env(Hashtbl.create 10))"), non da make_test_state(). La funzione
      che userebbe lo stato giusto, "make_prog_case_with_env", e' commentata
      nel file dei test. Bisogna riattivarla e usarla per iftests/whiletests,
      altrimenti "x", "y", "w" risultano non definite (=> D.top) e i
      risultati non corrispondono a quanto suggerito dai nomi dei test.

   2) compare_type nel tuo modulo Intervals decide "uguale"/"maggiore"/
      "minore" confrontando i bound a coppie (a vs c, b vs d), ma questo
      NON e' sound in generale:
        - due intervalli con stessi bound ma non singleton (es. Top,Top,
          o due variabili diverse con lo stesso range) vengono dichiarati
          "uguali" (0) invece che "ambigui" (2)
        - due intervalli che si sovrappongono (es. w=[0,10], x=[1,10])
          possono risultare "decisi" invece che ambigui
      Il criterio sound e':
        - definitivamente MAGGIORE  <=>  a > d  (min di x supera max di y)
        - definitivamente MINORE    <=>  b < c  (max di x e' sotto min di y)
        - definitivamente UGUALE    <=>  a = b = c = d (entrambi singleton
          coincidenti)
        - altrimenti: AMBIGUO (2)
      I valori sotto assumono questa versione corretta di compare_type.

   3) Il refine di e2 in eval_cond usa "negate_comp comp" per calcolare il
      vincolo sul secondo operando, ma li' serve il CONVERSO (converse_comp),
      non la negazione logica (vedi discussione precedente). Senza il fix,
      "while_4" collassa a Bottom a causa del refine su "y" col comparatore
      NotEquals.

   Con questi tre fix, i valori sotto sono quelli che il tuo interprete
   dovrebbe produrre.
   ===================================================================== *)



module Expected_Intervals : EXPECTED_VALUES with type t = Abstract_domains.Intervals.t = struct
  open Abstract_domains.Intervals
  type t = Abstract_domains.Intervals.t

  (* Helper per leggibilita' *)
  let interval lo hi = Interval (Int lo, Int hi)
  let to_pos_inf lo = Interval (Int lo, PosInf)
  (* let from_neg_inf hi = Interval (NegInf, Int hi) *)
  let top_val = Interval (NegInf, PosInf)
  let bottom_val = Bottom

  (* ---------------- Somma ---------------- *)
  let sum_1 = interval 2 20        (* x+x: [1,10]+[1,10] *)
  let sum_2 = interval (-9) 9      (* x+y: [1,10]+[-10,-1] *)
  let sum_3 = interval (-20) (-2)  (* y+y *)
  let sum_4 = interval 1 10        (* x+z *)
  let sum_5 = interval 0 0         (* z+z *)
  let sum_6 = interval 0 20        (* w+w *)
  let sum_7 = interval (-20) 0     (* k+k *)
  let sum_8 = interval (-10) 10    (* w+k *)
  let sum_9 = interval 1 20        (* w+x *)
  let sum_10 = interval (-10) 9    (* w+y *)
  let sum_11 = interval (-9) 10    (* k+x *)
  let sum_12 = interval (-20) (-1) (* k+y *)
  let sum_13 = interval (-9) 20    (* n+x *)
  let sum_14 = interval (-10) 10   (* n+z *)
  let sum_15 = interval (-20) 20   (* n+n *)
  let sum_16 = top_val             (* t+x *)
  let sum_17 = bottom_val          (* b+x *)
  let sum_18 = interval (-10) (-10) (* 10+(-20) *)

  (* ---------------- Sottrazione ---------------- *)
  let sub_1 = interval 2 20         (* x-y *)
  let sub_2 = interval (-9) 9       (* x-x *)
  let sub_3 = interval (-10) (-10)  (* 10-20 *)
  let sub_4 = interval (-10) 10     (* w-w *)
  let sub_5 = interval 1 10         (* z-y *)

  (* ---------------- Moltiplicazione ---------------- *)
  let mul_1 = interval 1 100       (* x*x *)
  let mul_2 = interval (-100) (-1) (* x*y *)
  let mul_3 = interval 1 100       (* y*y *)
  let mul_4 = interval 0 0         (* x*z *)
  let mul_5 = interval (-100) 0    (* w*y *)
  let mul_6 = interval (-100) 0    (* k*x *)
  let mul_7 = interval 0 0         (* n*z *)
  let mul_8 = interval (-100) 100  (* n*n *)
  let mul_9 = interval 0 0         (* t*z *)
  let mul_10 = bottom_val          (* b*x *)

  (* ---------------- Divisione ---------------- *)
  let div_1 = interval 0 10       (* x/x *)
  let div_2 = interval (-10) 0    (* x/y *)
  let div_3 = interval 0 10       (* y/y *)
  let div_4 = bottom_val          (* 10/z : divisione per {0} certa *)
  let div_5 = interval 0 10       (* x/w *)
  let div_6 = interval (-10) 0    (* x/k *)
  let div_7 = interval (-10) 10   (* x/n *)
  let div_8 = interval 0 0        (* z/x *)
  let div_9 = interval 0 0        (* z/y *)
  let div_10 = top_val            (* t/x *)
  let div_11 = interval (-10) 0   (* w/y *)
  let div_12 = interval (-10) 0   (* k/x *)

  (* ---------------- Negazione Unaria ---------------- *)
  let neg_1 = interval (-10) (-1)  (* -x *)
  let neg_2 = interval 1 10        (* -y *)
  let neg_3 = interval 0 0         (* -z *)
  let neg_4 = interval (-10) 0     (* -w *)
  let neg_5 = interval 0 10        (* -k *)
  let neg_6 = interval (-10) 10    (* -n *)
  let neg_7 = top_val              (* -t *)
  let neg_8 = bottom_val           (* -b *)
  let neg_9 = interval 1 10        (* --x *)
  let neg_10 = interval 2 20       (* x+(-y) *)

  (* ---------------- Random ---------------- *)
  let rand_1 = interval (-1) 10
  let rand_2 = interval 1 10
  let rand_3 = interval (-10) (-1)
  let rand_4 = interval 0 10
  let rand_5 = interval (-10) 0
  let rand_6 = interval 0 0

  (* ---------------- Assegnamenti ---------------- *)
  let assign_1 = interval 5 5
  let assign_2 = interval (-5) (-5)
  let assign_3 = interval 0 0
  let assign_4 = top_val   (* y = x, con x non definita -> Top *)
  let assign_5 = interval 1 10

  (* ---------------- Sequenze ---------------- *)
  let sequence_1_1 = interval 5 5
  let sequence_1_2 = interval (-3) (-3)

  (* ---------------- Skip ---------------- *)
  let skip_1 = interval 42 42

  (* ---------------- If ---------------- *)
  (* Nota: if_2 e if_4..if_12 assumono l'uso di make_test_state()
     (vedi punto 1 in testa al file) *)
  let if_1 = interval 1 1      (* x=5>0 deciso -> y=1 *)
  let if_2 = interval 1 1      (* x=[1,10] > y=[-10,-1] deciso -> k=1 *)
  let if_3 = interval 2 2      (* y=-3 > x=5 deciso falso -> k=2 *)
  let if_4 = interval (-5) 5   (* w,x si sovrappongono: ambiguo -> lub(5,-5) *)
  let if_5 = interval 0 1      (* ambiguo -> lub(1,0) *)
  let if_6 = interval (-1) 0   (* ambiguo -> lub(-1,0) *)
  let if_7 = interval (-100) 3  (* then k=3; else k=w*y con w narrowed=[0,9], y=[-10,-1] -> [-90,0]; lub *)
  let if_8 = interval 10 20    (* ambiguo -> lub(10,20) *)
  let if_9 = interval 7 7      (* m assegnata solo nel then *)
  let if_10 = interval (-7) (-7) (* m assegnata solo nell'else *)
  let if_11 = interval (-1) 100  (* if annidato, entrambi ambigui *)
  let if_12 = interval (-1) 1    (* And: x>y deciso vero, w>x ambiguo *)

  (* ---------------- While ---------------- *)
  let while_1_1 = interval 5 5   (* while mai eseguito: x resta 5 *)
  let while_1_2 = top_val        (* non usato dai test attuali *)
  let while_2_1 = interval 0 0   (* x=5; while x!=0 x=0 -> x=0 esatto *)
  let while_3_1 = to_pos_inf 0   (* perdita di precisione: [0,+Inf] *)
  let while_3_2 = to_pos_inf 0
  let while_4_1 = interval 1 1   (* x!=y deciso subito -> loop mai eseguito *)
  let while_4_2 = interval 2 2
  let while_4_3 = interval (-3) 5
end
