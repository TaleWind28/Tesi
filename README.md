# Interprete Astratto & Analizzatore Statico in OCaml

![OCaml](https://img.shields.io/badge/OCaml-4.12%2B-orange.svg)
![Dune](https://img.shields.io/badge/Build%20System-Dune-blue.svg)
![Test](https://img.shields.io/badge/Testing-Alcotest-green.svg)

Un tool di analisi statica ed interpretazione astratta scritto in OCaml per un linguaggio di programmazione imperativo. L'interprete è parametrizzato su **Domini Astratti** generici, includendo sia domini non-relazionali (5 varianti del dominio dei Segni e il dominio degli Intervalli) sia domini debolmente relazionali (il dominio delle **Zone** e il dominio degli **Ottagoni** basati su DBM). Calcola l'approssimazione corretta dell'esecuzione dei programmi mediante punto fisso, tecniche di **Widening**, **Narrowing** e **Raffinamento dei Vincoli**.

---

## 📋 Indice dei Contenuti

- [Panoramica](#-panoramica)
- [Caratteristiche Principali](#-caratteristiche-principali)
- [Domini Astratti](#-domini-astratti)
  - [Domini Non-Relazionali](#domini-non-relazionali)
  - [Domini Debolmente Relazionali (Zone e Ottagoni)](#domini-debolmente-relazionali-zone-e-ottagoni)
- [Struttura del Progetto](#-struttura-del-progetto)
- [Prerequisiti](#-prerequisiti)
- [Installazione](#-installazione)
- [Compilazione ed Esecuzione](#-compilazione-ed-esecuzione)
  - [Compilare il Progetto](#compilare-il-progetto)
  - [Eseguire il Programma Principale](#eseguire-il-programma-principale)
  - [Eseguire i Test Unitari (Alcotest)](#eseguire-i-test-unitari-alcotest)
  - [REPL Interattivo (utop)](#repl-interattivo-utop)
- [Sintassi del Linguaggio ed Esempi](#-sintassi-del-linguaggio-ed-esempi)
- [Licenza](#-licenza)

---

## 🔍 Panoramica

Questo progetto implementa un analizzatore statico basato sulla teoria dell'interpretazione astratta per analizzare programmi imperativi garantendo correttezza e terminazione.

L'analizzatore valuta programmi composti da:
- **Espressioni**: Costanti, variabili, operazioni aritmetiche (`+`, `-`, `*`, `/`), negazione unaria (`-x`) e scelte non deterministiche (`Random(min, max)`).
- **Condizioni**: Comparazioni relazionali (`=`, `<>`, `>`, `<`, `>=`, `<=`), logica booleana (`Not`, `And`, `Or`).
- **Comandi**: Assegnamento a variabili (`Assign`), sequenze di comandi (`Sequence`), istruzioni condizionali (`If`), filtri di guardia (`Filter`), no-op (`Skip`) e cicli (`While`).

Determina le proprietà di sicurezza, i limiti numerici delle variabili, i segni o le relazioni congiunte (differenze e somme ottagonali) nello stato finale di terminazione del programma.

---

## ✨ Caratteristiche Principali

- **Architettura Parametrica a Funtori**:
  - `NonRelationalAbsInterp (D : NonRelationalDomain)`: motore per domini non-relazionali che mappano identificatori a valori astratti indipendenti (`(string, D.t) Hashtbl.t`).
  - `WeakRelationalAbsInterp (D : WeakRelationalDomain)`: motore relazionale per vincoli tra variabili basato su matrici DBM globali.
- **Molteplici Domini Astratti**:
  - 5 varianti del Dominio dei Segni con diversi livelli di granularità.
  - Dominio degli Intervalli con estremi estesi (`-Inf`, `+Inf`).
  - Dominio delle Zone basato su matrici di vincoli di differenza (DBM).
  - Dominio degli Ottagoni basato su matrici 2n x 2n con chiusura forte.
- **Calcolo del Punto Fisso e Widening/Narrowing**:
  - Motore unificato `Shared_modules.Fixpoint` per accelerare la convergenza nei cicli `While` tramite **Widening** (`widen`).
  - Recupero della precisione persa post-convergenza tramite operatore di **Narrowing** (`narrow`).
- **Raffinamento delle Condizioni e Inconsistenze**:
  - Filtri e diramazioni (`If`, `Filter`) raffinano lo stato delle variabili tramite intersezione/least upper bound (`lub`) e vincoli di disuguaglianza.
  - Verifica delle contraddizioni e dei vincoli `NotEquals` per identificare rami irraggiungibili (`Bottom`).
- **Suite di Test Completa**:
  - **829 test automatizzati** basati su [Alcotest](https://github.com/mirage/alcotest) che coprono ogni operatore, costrutto sintattico e caso limite su tutti i domini (inclusi loop lockstep, swap di variabili, catene transitive a 5 nodi e branching condizionale).

---

## 📐 Domini Astratti

I domini astratti sono implementati in `lib/abstract_domains.ml`:

### Domini Non-Relazionali

1. **`ExtendedSigns`**: Dominio completo dei segni a 8 elementi:
   `{ Top, >0, >=0, 0, <=0, <0, !=0, Bottom }`
2. **`SimpleSigns`**: Dominio a 5 elementi con lo zero compreso nei semipiani:
   `{ Top, >=0, 0, <=0, Bottom }`
3. **`SimplifiedSigns`**: Dominio a 5 elementi con segni stretti:
   `{ Top, >0, 0, <0, Bottom }`
4. **`Signs`**: Dominio minimale a 4 elementi senza zero esplicito:
   `{ Top, >0, <0, Bottom }`
5. **`StrangeSigns`**: Dominio asimmetrico a 5 elementi:
   `{ Top, >=0, 0, <0, Bottom }`
6. **`Intervals`**: Dominio degli intervalli `[lo, hi]` con estremi in `{-Inf, Int n, +Inf}`, aritmetica per intervalli, estensione non deterministica e raffinamento dei limiti.

### Domini Debolmente Relazionali (Zone e Ottagoni)

7. **`Zones`**: Dominio relazionale basato su **Difference Bound Matrices (DBM)** per tracciare vincoli del tipo:
   - Differenze tra coppie di variabili: `x - y <= c`
   - Limiti individuali rispetto alla variabile zero `v0`: `x <= c` e `x >= c`
   - Chiusura canonica dei cammini minimi mediante algoritmo di **Floyd-Warshall**
   - Rilevamento di cicli di peso negativo per identificare stati irraggiungibili (`Bottom`)
   - Operazioni di riassegnamento (`assign_const`, `assign_var`), traslazione (`shift_var`), widening e narrowing relazionali.

8. **`Octagons`**: Dominio relazionale avanzato per vincoli del tipo `+/- x +/- y <= c`:
   - Rappresentazione tramite matrice DBM estesa `2n x 2n` per rappresentare le forme positive `+x` e negative `-x`.
   - **Chiusura Forte (Strong Closure)** che combina la chiusura transitiva dei cammini minimi con la normalizzazione unaria `m[i,j] <= (m[i, not i] + m[not j, j]) / 2`.
   - Supporto ad assegnamenti affini esatti sia concordi che discordi: `y := x + c` e `y := -x + c`.
   - Filtro di vincoli unari, somme concordi (`x + y <= c`), somme negative (`-x - y <= c`) e differenze inverse (`-x + y <= c`).

---

## 📁 Struttura del Progetto

```text
.
├── bin/
│   ├── dune                  # Configurazione Dune per l'eseguibile main
│   └── main.ml               # Entry point dimostrativo con analisi su tutti i domini
├── lib/
│   ├── abstract_domains.ml   # Firme (NonRelationalDomain, WeakRelationalDomain) e moduli dei domini
│   ├── dune                  # Configurazione Dune per la libreria (tesi_lib)
│   ├── interpeters.ml        # Funtori di analisi statica (NonRelationalAbsInterp, WeakRelationalAbsInterp)
│   ├── shared_modules.ml     # Moduli condivisi: IntervalArith, DBMOperations, VariableRetrieval, SyntaxUtils, Fixpoint
│   └── syntax.ml             # Abstract Syntax Tree (bop, uop, exp, cond, cmd)
├── test/
│   ├── dune                  # Configurazione Dune per la suite di test
│   ├── oracles.ml            # Risultati attesi (oracoli) specifici per ogni dominio
│   └── test_suite.ml         # Suite di 829 test Alcotest per tutti i domini
├── dune-project              # File di progetto Dune
└── README.md                 # Documentazione del progetto
```

### Moduli Condivisi (`lib/shared_modules.ml`)

- **`IntervalArith`**: Definizione del tipo `bound` esteso (`NegInf`, `Int`, `PosInf`), tipo `value` come intervallo e funzioni aritmetiche con raffinamento.
- **`DBMOperations`**: Definizione della struttura DBM, algoritmi di Floyd-Warshall per la chiusura dei cammini minimi, rilevamento di cicli negativi, `lub_matrix`, `glb_matrix`, `widen_matrix` e `narrow_matrix`.
- **`VariableRetrieval`**: Estrazione statica dell'insieme delle variabili di programma da espressioni, condizioni e comandi (`get_all_var`).
- **`SyntaxUtils`**: Funzioni logiche e sintattiche riutilizzabili: `negate_comp`, `inv_comp` e `negate_cond` (con leggi di De Morgan).
- **`Fixpoint`**: Calcolo generico dell'invariante di ciclo tramite iterazione ascendente (Widening) e iterazione discendente (Narrowing).

---

## 🛠️ Prerequisiti

Per compilare ed eseguire il progetto sono necessari:

- **OCaml** (>= 4.12)
- **Opam** (OCaml Package Manager)
- **Dune** (>= 3.0)
- **Alcotest** (per l'esecuzione dei test unitari)
- **Utop** (opzionale, per esplorazione interattiva tramite REPL)

---

## 📥 Installazione

1. **Clonare il repository**:
   ```bash
   git clone https://github.com/TaleWind28/Tesi.git
   cd Tesi
   ```

2. **Inizializzare o aggiornare l'ambiente Opam**:
   ```bash
   opam switch create . 4.14.0  # Oppure utilizza uno switch esistente
   eval $(opam env)
   ```

3. **Installare le dipendenze**:
   ```bash
   opam install dune alcotest utop
   ```

---

## 🚀 Compilazione ed Esecuzione

### Compilare il Progetto

```bash
dune build
# Oppure: opam exec -- dune build
```

### Eseguire il Programma Principale

Esegue lo script di dimostrazione principale (`bin/main.ml`):
```bash
dune exec bin/main.exe
# Oppure: opam exec -- dune exec bin/main.exe
```

### Eseguire i Test Unitari (Alcotest)

Esegue la suite completa di **829 test** automatizzati su tutti i domini:
```bash
dune runtest
# Oppure: opam exec -- dune runtest
```

#### Flag Utili per il Testing:

- **Forzare la riesecuzione ignorando la cache**:
  ```bash
  dune runtest -f
  ```
- **Modalità Watch (riesegue i test automaticamente ad ogni salvataggio)**:
  ```bash
  dune runtest -f -w
  ```
- **Eseguire un gruppo o test specifico per nome**:
  ```bash
  dune exec ./test/test_suite.exe -- test "Zones: Cicli While"
  ```
- **Elencare tutti i test disponibili**:
  ```bash
  dune exec ./test/test_suite.exe -- list
  ```

### REPL Interattivo (utop)

Per sperimentare in modo interattivo con i domini astratti:

1. Avviare `utop` caricando la libreria di progetto:
   ```bash
   opam exec -- dune utop lib
   ```

2. All'interno di `utop`:
   ```ocaml
   open Syntax;;
   open Abstract_domains;;
   open Interpeters;;

   (* Analisi con il Dominio degli Intervalli *)
   let p1 = Sequence (Assign ("x", Const 0), While (Comparison (Var "x", Smaller, Const 10), Assign ("x", BinaryOperation (Var "x", Add, Const 2))));;
   IntervalInterp.eval p1;;

   (* Analisi con il Dominio delle Zone (DBM) *)
   let res_zone = ZoneInterp.eval p1;;
   ZoneInterp.print_result res_zone;;

   (* Analisi con il Dominio degli Ottagoni (DBM 2nx2n) *)
   let res_oct = OctagonInterp.eval p1;;
   OctagonInterp.print_result res_oct;;
   ```

---

## 💻 Sintassi del Linguaggio ed Esempi

I programmi vengono costruiti mediante i costruttori AST definiti in `lib/syntax.ml`:

```ocaml
open Syntax
open Interpeters

(* Programma: 
   x = Random(1, 5);
   y = -x + 10;
   z = x + 2;
*)
let program =
  Sequence (
    Assign ("x", Random (1, 5)),
    Sequence (
      Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 10)),
      Assign ("z", BinaryOperation (Var "x", Add, Const 2))
    )
  )

let () =
  (* Valutazione con Intervalli *)
  let res_intervals = IntervalInterp.eval program in
  print_endline "Risultato con Intervalli:";
  IntervalInterp.outputStatePrinter res_intervals;

  (* Valutazione con il Dominio delle Zone (DBM) *)
  let res_zones = ZoneInterp.eval program in
  print_endline "\nRisultato con Zone (DBM):";
  ZoneInterp.print_result res_zones;

  (* Valutazione con il Dominio degli Ottagoni *)
  let res_octagons = OctagonInterp.eval program in
  print_endline "\nRisultato con Ottagoni:";
  OctagonInterp.print_result res_octagons
```

---

## 📜 Licenza

Distribuito sotto licenza MIT. Per maggiori informazioni vedere il file `LICENSE` (se presente).
