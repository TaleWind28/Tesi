(* open Syntax

(* ------------------------------------------------------------------ *)
(*  Signature del dominio                                             *)
(* ------------------------------------------------------------------ *)

module type WeakReletionalDomain = sig
  type t
  val bottom : t
  val init : ide list -> t
  val is_bottom : t -> bool
  val normalize : t -> t
  val leq : t -> t -> bool
  val lub : t -> t -> t
  val glb : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t
  val assign : ide -> exp -> t -> t
  val forget : ide -> t -> t
  val filter : cond -> t -> t
  val to_string : t -> string
  val print : t -> unit 
end *)

(* ------------------------------------------------------------------ *)
(*  Implementazione: Zone / DBM                                       *)
(* ------------------------------------------------------------------ *)

(* module Zones : WeakReletionalDomain = struct

  (* --- Estremi con +infinito ---------------------------------------- *)

  type bound = Fin of int | Inf

  let b_leq a b =
    match a, b with
    | _, Inf -> true
    | Inf, _ -> false
    | Fin x, Fin y -> x <= y

  let b_min a b = if b_leq a b then a else b
  let b_max a b = if b_leq a b then b else a

  let b_add a b =
    match a, b with
    | Inf, _ | _, Inf -> Inf
    | Fin x, Fin y -> Fin (x + y)

  (* Riallinea due DBM su un ambiente comune (unione delle variabili),
     riempiendo con Inf (nessun vincolo) le variabili mancanti. Serve
     per le operazioni binarie, nel caso i due ambienti non coincidano. *)
  let merge_env (d1 : dbm) (d2 : dbm) : dbm * dbm =
    if d1.env = d2.env then (d1, d2)
    else begin
      let vars =
        List.sort_uniq compare (List.map fst d1.env @ List.map fst d2.env)
      in
      let n = List.length vars in
      let env = List.mapi (fun i x -> (x, i)) vars in
      let build (d : dbm) =
        let s = n + 1 in
        let m =
          Array.init s (fun i -> Array.init s (fun j -> if i = j then Fin 0 else Inf))
        in
        let old_to_new = Array.make (d.n + 1) (-1) in
        List.iter
          (fun (x, i) ->
            match index_of env x with
            | Some ni -> old_to_new.(i) <- ni
            | None -> ())
          d.env;
        old_to_new.(d.n) <- n;
        (* la variabile zero va sempre nell'ultimo indice del nuovo spazio *)
        for i = 0 to d.n do
          for j = 0 to d.n do
            let ni = old_to_new.(i) and nj = old_to_new.(j) in
            if ni >= 0 && nj >= 0 then m.(ni).(nj) <- d.mat.(i).(j)
          done
        done;
        { env; n; mat = m }
      in
      (build d1, build d2)
    end

  (* --- Operazioni base del dominio ------------------------------------ *)

  let leq a b =
    match (a, b) with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Z d1, Z d2 ->
        let d1, d2 = merge_env d1 d2 in
        let s = d1.n + 1 in
        let ok = ref true in
        for i = 0 to s - 1 do
          for j = 0 to s - 1 do
            if not (b_leq d1.mat.(i).(j) d2.mat.(i).(j)) then ok := false
          done
        done;
        !ok

  let lub a b =
    match (a, b) with
    | Bottom, x | x, Bottom -> x
    | Z d1, Z d2 ->
        let d1, d2 = merge_env d1 d2 in
        let s = d1.n + 1 in
        let m =
          Array.init s (fun i ->
              Array.init s (fun j -> b_max d1.mat.(i).(j) d2.mat.(i).(j)))
        in
        (* il massimo puntuale di due DBM chiuse è già chiuso: la
           chiusura qui è ridondante ma innocua, e mantiene l'invariante
           in modo uniforme e robusto anche in caso di modifiche future *)
        close_dbm { d1 with mat = m }

  let glb a b =
    match (a, b) with
    | Bottom, _ | _, Bottom -> Bottom
    | Z d1, Z d2 ->
        let d1, d2 = merge_env d1 d2 in
        let s = d1.n + 1 in
        let m =
          Array.init s (fun i ->
              Array.init s (fun j -> b_min d1.mat.(i).(j) d2.mat.(i).(j)))
        in
        (* il minimo puntuale può violare la disuguaglianza triangolare:
           la richiusura è qui necessaria *)
        close_dbm { d1 with mat = m }

  let widen a b =
    match (a, b) with
    | Bottom, x -> x
    | x, Bottom -> x
    | Z d1, Z d2 ->
        let d1, d2 = merge_env d1 d2 in
        let s = d1.n + 1 in
        let m =
          Array.init s (fun i ->
              Array.init s (fun j ->
                  if b_leq d2.mat.(i).(j) d1.mat.(i).(j) then d2.mat.(i).(j) else Inf))
        in
        close_dbm { d1 with mat = m }

  let narrow a b =
    match (a, b) with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Z d1, Z d2 ->
        let d1, d2 = merge_env d1 d2 in
        let s = d1.n + 1 in
        let m =
          Array.init s (fun i ->
              Array.init s (fun j ->
                  match d1.mat.(i).(j) with Inf -> d2.mat.(i).(j) | v -> v))
        in
        close_dbm { d1 with mat = m }

  (* --- forget ---------------------------------------------------------- *)

  let forget (x : ide) (t : t) : t =
    match t with
    | Bottom -> Bottom
    | Z d -> (
        (* la specifica richiede la chiusura preventiva della DBM *)
        match close_dbm d with
        | Bottom -> Bottom
        | Z d ->
            let i = idx d.env x and s = d.n + 1 in
            let m = Array.map Array.copy d.mat in
            for j = 0 to s - 1 do
              if j <> i then begin
                m.(i).(j) <- Inf;
                m.(j).(i) <- Inf
              end
            done;
            Z { d with mat = m })

  (* --- Valutazione a intervalli (per assegnamenti/filtri non lineari) - *)

  type itv = { lo : int option; hi : int option } (* None = infinito *)

  let itv_top = { lo = None; hi = None }

  let opt_add a b = match (a, b) with Some x, Some y -> Some (x + y) | _ -> None
  let opt_neg = function Some x -> Some (-x) | None -> None

  let itv_neg i = { lo = opt_neg i.hi; hi = opt_neg i.lo }
  let itv_add i1 i2 = { lo = opt_add i1.lo i2.lo; hi = opt_add i1.hi i2.hi }
  let itv_sub i1 i2 = itv_add i1 (itv_neg i2)

  let itv_mul i1 i2 =
    match (i1.lo, i1.hi, i2.lo, i2.hi) with
    | Some a, Some b, Some c, Some d ->
        let ps = [ a * c; a * d; b * c; b * d ] in
        { lo = Some (List.fold_left min (List.hd ps) ps);
          hi = Some (List.fold_left max (List.hd ps) ps) }
    | _ -> itv_top (* approssimazione prudente in presenza di estremi infiniti *)

  let itv_div i1 i2 =
    let contains_zero =
      (match i2.lo with Some l -> l <= 0 | None -> true)
      && match i2.hi with Some h -> h >= 0 | None -> true
    in
    if contains_zero then itv_top (* divisione potenzialmente per 0: top *)
    else
      match (i1.lo, i1.hi, i2.lo, i2.hi) with
      | Some a, Some b, Some c, Some d ->
          let qs = [ a / c; a / d; b / c; b / d ] in
          { lo = Some (List.fold_left min (List.hd qs) qs);
            hi = Some (List.fold_left max (List.hd qs) qs) }
      | _ -> itv_top

  let bounds_of (d : dbm) (x : ide) : int option * int option =
    let i = idx d.env x and z = d.n in
    let hi = match d.mat.(i).(z) with Fin c -> Some c | Inf -> None in
    let lo = match d.mat.(z).(i) with Fin c -> Some (-c) | Inf -> None in
    (lo, hi)

  let rec eval_itv (d : dbm) (e : exp) : itv =
    match e with
    | Const c -> { lo = Some c; hi = Some c }
    | Var x ->
        let lo, hi = bounds_of d x in
        { lo; hi }
    | Random (a, b) -> { lo = Some (min a b); hi = Some (max a b) }
    | UnaryOperation (Negation, e1) -> itv_neg (eval_itv d e1)
    | BinaryOperation (e1, op, e2) ->
        let i1 = eval_itv d e1 and i2 = eval_itv d e2 in
        (match op with
        | Add -> itv_add i1 i2
        | Sub -> itv_sub i1 i2
        | Mul -> itv_mul i1 i2
        | Div -> itv_div i1 i2)

  (* --- assign ----------------------------------------------------------- *)

  (* trasla la variabile x di c, MANTENENDO tutte le sue relazioni con le
     altre variabili: x_new = x_old + c. Usata per x := x + c, esatta. *)
  let shift (d : dbm) (x : ide) (c : int) : dbm =
    let i = idx d.env x and s = d.n + 1 in
    let m = Array.map Array.copy d.mat in
    for j = 0 to s - 1 do
      if j <> i then begin
        m.(i).(j) <- b_add m.(i).(j) (Fin c);
        m.(j).(i) <- b_add m.(j).(i) (Fin (-c))
      end
    done;
    { d with mat = m }

  (* riconosce le forme y, y+c, c+y, y-c: le uniche traslazioni ESATTE
     rappresentabili in una DBM (coefficiente 1 sulla variabile) *)
  let is_translation (e : exp) : (ide * int) option =
    match e with
    | Var y -> Some (y, 0)
    | BinaryOperation (Var y, Add, Const c) -> Some (y, c)
    | BinaryOperation (Const c, Add, Var y) -> Some (y, c)
    | BinaryOperation (Var y, Sub, Const c) -> Some (y, -c)
    | _ -> None

  let assign (x : ide) (e : exp) (t : t) : t =
    match t with
    | Bottom -> Bottom
    | Z d ->
        ignore (idx d.env x);
        (match is_translation e with
        | Some (y, c) when y = x ->
            (* x := x + c : traslazione esatta, nessuna informazione persa *)
            Z (shift d x c)
        | Some (y, c) -> (
            (* x := y + c, con y variabile diversa da x: si dimentica x,
               si impone il legame esatto con y, poi si richiude per
               propagare le relazioni di x con tutte le altre variabili *)
            match forget x (Z d) with
            | Bottom -> Bottom
            | Z d' ->
                let ix = idx d'.env x and iy = idx d'.env y in
                let m = Array.map Array.copy d'.mat in
                m.(ix).(iy) <- Fin c;
                m.(iy).(ix) <- Fin (-c);
                close_dbm { d' with mat = m })
        | None -> (
            (* assegnamento generico non affine (o affine non esatto per
               le zone): si valuta un intervallo sicuro per e usando lo
               stato corrente, si dimentica x e si impongono i nuovi
               limiti come vincoli unari (rispetto alla variabile zero) *)
            let itv = eval_itv d e in
            match forget x (Z d) with
            | Bottom -> Bottom
            | Z d' ->
                let ix = idx d'.env x and iz = d'.n in
                let m = Array.map Array.copy d'.mat in
                (match itv.hi with Some h -> m.(ix).(iz) <- Fin h | None -> ());
                (match itv.lo with Some l -> m.(iz).(ix) <- Fin (-l) | None -> ());
                close_dbm { d' with mat = m }))

  (* --- filter ------------------------------------------------------------ *)

  let neg_comparator = function
    | Equals -> NotEquals
    | NotEquals -> Equals
    | Bigger -> SmallerEquals
    | SmallerEquals -> Bigger
    | Smaller -> BiggerEquals
    | BiggerEquals -> Smaller

  let rec neg_cond = function
    | Boolean b -> Boolean (not b)
    | Comparison (e1, cmp, e2) -> Comparison (e1, neg_comparator cmp, e2)
    | Not c -> c
    | And (c1, c2) -> Or (neg_cond c1, neg_cond c2)
    | Or (c1, c2) -> And (neg_cond c1, neg_cond c2)

  (* riconosce espressioni della forma "x + k" o "k" (al più una
     variabile, con coefficiente 1): sono le uniche esprimibili come
     vincolo di differenza esatto nella DBM *)
  let rec as_affine (e : exp) : (ide option * int) option =
    match e with
    | Const c -> Some (None, c)
    | Var x -> Some (Some x, 0)
    | UnaryOperation (Negation, e1) -> (
        match as_affine e1 with Some (None, c) -> Some (None, -c) | _ -> None)
    | BinaryOperation (e1, Add, e2) -> (
        match (as_affine e1, as_affine e2) with
        | Some (None, c1), Some (None, c2) -> Some (None, c1 + c2)
        | Some (Some x, c1), Some (None, c2) -> Some (Some x, c1 + c2)
        | Some (None, c1), Some (Some x, c2) -> Some (Some x, c1 + c2)
        | _ -> None)
    | BinaryOperation (e1, Sub, e2) -> (
        match (as_affine e1, as_affine e2) with
        | Some (None, c1), Some (None, c2) -> Some (None, c1 - c2)
        | Some (Some x, c1), Some (None, c2) -> Some (Some x, c1 - c2)
        | _ -> None)
    | _ -> None

  (* impone il vincolo Vi - Vj <= k (solo se più stretto dell'attuale) e
     richiude *)
  let tighten (d : dbm) (i : int) (j : int) (k : int) : t =
    let m = Array.map Array.copy d.mat in
    let nv = Fin k in
    if b_leq nv m.(i).(j) then m.(i).(j) <- nv;
    close_dbm { d with mat = m }

  (* applica il vincolo  (v1 + k1) cmp (v2 + k2)  *)
  let apply_diff_constraint (d : dbm) v1 k1 cmp v2 k2 : t =
    let z = d.n in
    let i1 = match v1 with Some x -> idx d.env x | None -> z in
    let i2 = match v2 with Some x -> idx d.env x | None -> z in
    if i1 = i2 then
      (* stessa variabile (o entrambe costanti): si riduce al confronto
         tra le costanti k1 e k2 *)
      let ok =
        match cmp with
        | Equals -> k1 = k2
        | NotEquals -> k1 <> k2
        | Bigger -> k1 > k2
        | BiggerEquals -> k1 >= k2
        | Smaller -> k1 < k2
        | SmallerEquals -> k1 <= k2
      in
      if ok then Z d else Bottom
    else
      (* v1 + k1 cmp v2 + k2  <=>  V(i1) - V(i2) cmp (k2 - k1) *)
      let k = k2 - k1 in
      match cmp with
      | SmallerEquals -> tighten d i1 i2 k
      | Smaller -> tighten d i1 i2 (k - 1)
      | BiggerEquals -> tighten d i2 i1 (-k)
      | Bigger -> tighten d i2 i1 (-k - 1)
      | Equals -> (
          match tighten d i1 i2 k with
          | Bottom -> Bottom
          | Z d' -> tighten d' i2 i1 (-k))
      | NotEquals ->
          (* non rappresentabile esattamente nelle zone (creerebbe un
             "buco" convesso): si lascia il vincolo inalterato,
             approssimazione sicura *)
          Z d

  let definitely_false cmp (i1 : itv) (i2 : itv) =
    let lt a b = match (a, b) with Some x, Some y -> x < y | _ -> false in
    let gt a b = match (a, b) with Some x, Some y -> x > y | _ -> false in
    let le a b = match (a, b) with Some x, Some y -> x <= y | _ -> false in
    let ge a b = match (a, b) with Some x, Some y -> x >= y | _ -> false in
    match cmp with
    | SmallerEquals -> gt i1.lo i2.hi
    | Smaller -> ge i1.lo i2.hi
    | BiggerEquals -> lt i1.hi i2.lo
    | Bigger -> le i1.hi i2.lo
    | Equals -> gt i1.lo i2.hi || lt i1.hi i2.lo
    | NotEquals -> false

  let rec filter (c : cond) (t : t) : t =
    match t with
    | Bottom -> Bottom
    | Z d -> (
        match c with
        | Boolean true -> Z d
        | Boolean false -> Bottom
        | Not c1 -> filter (neg_cond c1) (Z d)
        | And (c1, c2) -> filter c2 (filter c1 (Z d))
        | Or (c1, c2) -> lub (filter c1 (Z d)) (filter c2 (Z d))
        | Comparison (e1, cmp, e2) -> (
            match (as_affine e1, as_affine e2) with
            | Some (v1, k1), Some (v2, k2) -> apply_diff_constraint d v1 k1 cmp v2 k2
            | _ ->
                (* fallback per espressioni non affini: si rileva
                   l'infeasibilità tramite intervalli, senza raffinare *)
                let i1 = eval_itv d e1 and i2 = eval_itv d e2 in
                if definitely_false cmp i1 i2 then Bottom else Z d))

  (* --- stampa -------------------------------------------------------------- *)

  let to_string (t : t) : string =
    match t with
    | Bottom -> "bottom\n"
    | Z d ->
        let s = d.n + 1 in
        let name i =
          if i = d.n then None
          else Some (fst (List.find (fun (_, j) -> j = i) d.env))
        in
        let buf = Buffer.create 256 in
        for i = 0 to s - 1 do
          for j = 0 to s - 1 do
            if i <> j then
              match d.mat.(i).(j) with
              | Inf -> ()
              | Fin c -> (
                  let line =
                    match (name i, name j) with
                    | Some x, Some y -> Printf.sprintf "%s - %s <= %d" x y c
                    | Some x, None -> Printf.sprintf "%s <= %d" x c
                    | None, Some y -> Printf.sprintf "%s >= %d" y (-c)
                    | None, None -> ""
                  in
                  if line <> "" then Buffer.add_string buf (line ^ "\n"))
          done
        done;
        if Buffer.length buf = 0 then "true\n" else Buffer.contents buf

  let print (t : t) : unit = print_string (to_string t)
end *)
