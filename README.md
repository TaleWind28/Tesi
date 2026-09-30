# Interprete Astratto & Analizzatore Statico in OCaml

![OCaml](https://img.shields.io/badge/OCaml-4.12%2B-orange.svg)
![Dune](https://img.shields.io/badge/Build%20System-Dune-blue.svg)
![Test](https://img.shields.io/badge/Testing-Alcotest%20(1121%20tests)-green.svg)

Un tool di analisi statica ed interpretazione astratta scritto in OCaml per un linguaggio di programmazione imperativo. L'interprete include un **Frontend con Lexer e Parser BNF** per analizzare programmi sia da stringa che da file sorgente, ed è parametrizzato su **Domini Astratti** generici: domini non-relazionali (5 varianti del dominio dei Segni e il dominio degli Intervalli) e domini debolmente relazionali (il dominio delle **Zone** e il dominio degli **Ottagoni** basati su DBM). Calcola l'approssimazione corretta dell'esecuzione dei programmi mediante punto fisso, tecniche di **Widening**, **Narrowing** e **Raffinamento dei Vincoli**.

---

## 📋 Indice dei Contenuti

- [Panoramica](#-panoramica)
- [Caratteristiche Principali](#-caratteristiche-principali)
- [Sintassi e Grammatica BNF (Parser)](#-sintassi-e-grammatica-bnf-parser)
- [Domini Astratti](#-domini-astratti)
  - [Domini Non-Relazionali](#domini-non-relazionali)
  - [Domini Debolmente Relazionali (Zone e Ottagoni)](#domini-debolmente-relazionali-zone-e-ottagoni)
- [Struttura del Progetto](#-struttura-del-progetto)
- [Prerequisiti](#-prerequisiti)
- [Installazione](#-installazione)
- [Compilazione ed Esecuzione](#-compilazione-ed-esecuzione)
  - [Compilare il Progetto](#compilare-il-progetto)
  - [Eseguire il Programma Principale (CLI)](#eseguire-il-programma-principale-cli)
  - [Eseguire i Test Unitari (Alcotest - 1121 Test)](#eseguire-i-test-unitari-alcotest---1121-test)
  - [REPL Interattivo (utop)](#repl-interattivo-utop)
- [Esempi di Utilizzo](#-esempi-di-utilizzo)
  - [Esempio 1: Analisi da Codice Sorgente Testuale (Parser)](#esempio-1-analisi-da-codice-sorgente-testuale-parser)
  - [Esempio 2: Costruzione Diretta dell'AST in OCaml](#esempio-2-costruzione-diretta-dellast-in-ocaml)
- [Licenza](#-licenza)

---

## 🔍 Panoramica

Questo progetto implementa un analizzatore statico basato sulla teoria dell'interpretazione astratta per analizzare programmi imperativi garantendo correttezza e terminazione.

L'analizzatore valuta programmi composti da:
- **Espressioni**: Costanti numeriche, identificatori di variabile, operazioni aritmetiche (`+`, `-`, `*`, `/`), negazione unaria (`-x`), operatori di incremento/decremento (`inc(e)`, `dec(e)`, `x++`, `x--`) e scelte non deterministiche (`nondet(min, max)` o `Random(min, max)`).
- **Condizioni**: Comparazioni relazionali (`=`, `==`, `<>`, `!=`, `>`, `<`, `>=`, `<=`), logica booleana (`not`, `and`, `or`) e costanti booleane (`true`, `false`).
- **Comandi**: Assegnamento a variabili (`x = e`), sequenze di comandi (`;`), blocchi di istruzioni racchiusi tra parentesi graffe `{ ... }` o `begin ... end`, istruzioni condizionali (`if cond then C1 else C2`), filtri di guardia (`cond ?` o `filter(cond)`), no-op (`Skip`) e cicli (`while cond do C`).

Determina le proprietà di sicurezza, i limiti numerici delle variabili, i segni o le relazioni congiunte (differenze e somme ottagonali) nello stato finale di terminazione del programma.

---

## ✨ Caratteristiche Principali

- **Frontend con Analizzatore Lessicale e Sintattico (Parser BNF)**:
  - Lexer e Recursive Descent Parser deterministico con lookahead O(1) e backtracking controllato.
  - Parsing di programmi direttamente da file sorgente su disco (`parse_cmd_from_file`), da stringhe (`parse_cmd`) o da canali di input.
  - Tracciamento dettagliato di riga e colonna per messaggi d'errore precisi (`ParseError`).
  - Pretty-printer integrato per rigenerare il codice sorgente indentato dall'AST (`string_of_cmd`).
- **Architettura Parametrica a Funtori**:
  - `NonRelationalAbsInterp (D : NonRelationalDomain)`: motore per domini non-relazionali che mappano identificatori a valori astratti indipendenti (`(string, D.t) Hashtbl.t`).
  - `WeakRelationalAbsInterp (D : WeakRelationalDomain)`: motore relazionale per vincoli tra coppie di variabili basato su matrici DBM globali.
- **Molteplici Domini Astratti**:
  - 5 varianti del Dominio dei Segni con diversi livelli di granularità (da 4 a 8 elementi).
  - Dominio degli Intervalli con estremi estesi (`-Inf`, `+Inf`), aritmetica d'intervallo e raffinamento.
  - Dominio delle Zone basato su Difference Bound Matrices (DBM).
  - Dominio degli Ottagoni basato su matrici 2n x 2n con chiusura forte.
- **Calcolo del Punto Fisso e Widening/Narrowing**:
  - Motore unificato `Shared_modules.Fixpoint` per accelerare la convergenza nei cicli `While` tramite **Widening** (`widen`).
  - Recupero della precisione persa post-convergenza tramite operatore di **Narrowing** (`narrow`).
- **Raffinamento delle Condizioni e Inconsistenze**:
  - Filtri e diramazioni (`If`, `Filter`) raffinano lo stato delle variabili tramite intersezione/least upper bound (`lub`) e vincoli di disuguaglianza.
  - Rilevamento automatico di rami e stati irraggiungibili (`Bottom` o cicli di peso negativo nel grafo dei vincoli).
- **Suite di Test Completa**:
  - **1121 test automatizzati** basati su [Alcotest](https://github.com/mirage/alcotest) che coprono ogni operatore, costrutto sintattico del parser BNF, sfide relazionali (lockstep while, swap di variabili, catene transitive a 5 nodi), narrowing e aritmetica estrema.

---

## 📜 Sintassi e Grammatica BNF (Parser)

Il modulo `lib/parser.ml` implementa un analizzatore per la seguente grammatica formale:

```text
c    ::= Skip 
       | ide = E 
       | C ; C 
       | if cond then C else C 
       | while cond do C 
       | cond ? 
       | filter(cond)
       | { C }
       | begin C end

cond ::= E comp E 
       | bool 
       | not cond 
       | cond and cond 
       | cond or cond

E    ::= int 
       | Ide 
       | E bop E 
       | uop E 
       | nondet(E, E) | Random(E, E)
       | inc(E) | E++
       | dec(E) | E--

comp ::= > | >= | < | <= | = | == | != | <>
bop  ::= + | - | * | /
uop  ::= -
Ide  ::= [a-zA-Z_][a-zA-Z0-9_]*
int  ::= [-]?[0-9]+
bool ::= true | false
```

### Funzioni Esportate dal Modulo `Parser`

- **Parsing di Comandi**:
  - `Parser.parse_cmd : string -> Syntax.cmd` (alias di `parse_cmd_from_string`)
  - `Parser.parse_cmd_from_file : string -> Syntax.cmd`
  - `Parser.parse_cmd_from_channel : in_channel -> Syntax.cmd`
- **Parsing di Espressioni e Condizioni**:
  - `Parser.parse_exp : string -> Syntax.exp`
  - `Parser.parse_cond : string -> Syntax.cond`
- **Pretty Printing**:
  - `Parser.string_of_cmd : Syntax.cmd -> string`
  - `Parser.string_of_exp : Syntax.exp -> string`
  - `Parser.string_of_cond : Syntax.cond -> string`
  - `Parser.string_of_parse_error : ParseError -> string`

---

## 📐 Domini Astratti

I domini astratti sono implementati in `lib/abstract_domains.ml` e gli interpreti concreti in `lib/interpeters.ml`:

### Domini Non-Relazionali

1. **`ExtendedSigns`** (`ExtendedSignInterp`): Dominio completo dei segni a 8 elementi:
   `{ Top, >0, >=0, 0, <=0, <0, !=0, Bottom }`
2. **`SimpleSigns`** (`SimpleSignInterp`): Dominio a 5 elementi con lo zero compreso nei semipiani:
   `{ Top, >=0, 0, <=0, Bottom }`
3. **`SimplifiedSigns`** (`SimplifiedSignInterp`): Dominio a 5 elementi con segni stretti:
   `{ Top, >0, 0, <0, Bottom }`
4. **`Signs`** (`SignInterp` / ReducedSigns): Dominio minimale a 4 elementi senza zero esplicito:
   `{ Top, >0, <0, Bottom }`
5. **`StrangeSigns`** (`StrangeSignInterp`): Dominio asimmetrico a 5 elementi:
   `{ Top, >=0, 0, <0, Bottom }`
6. **`Intervals`** (`IntervalInterp`): Dominio degli intervalli `[lo, hi]` con estremi in `{-Inf, Int n, +Inf}`, aritmetica per intervalli, estensione non deterministica e raffinamento dei limiti.

### Domini Debolmente Relazionali (Zone e Ottagoni)

7. **`Zones`** (`ZoneInterp`): Dominio relazionale basato su **Difference Bound Matrices (DBM)** per tracciare vincoli del tipo:
   - Differenze tra coppie di variabili: `x - y <= c`
   - Limiti individuali rispetto alla variabile zero canonica `v0`: `x <= c` e `x >= c`
   - Chiusura canonica dei cammini minimi mediante algoritmo di **Floyd-Warshall**
   - Rilevamento di cicli di peso negativo per identificare stati irraggiungibili (`Bottom`)
   - Operazioni di riassegnamento (`assign_const`, `assign_var`), traslazione (`shift_var`), widening e narrowing relazionali.

8. **`Octagons`** (`OctagonInterp`): Dominio relazionale avanzato per vincoli del tipo `+/- x +/- y <= c`:
   - Rappresentazione tramite matrice DBM estesa `2n x 2n` per rappresentare simultaneamente le forme positive `+x` e negative `-x`.
   - **Chiusura Forte (Strong Closure)** che combina la chiusura transitiva dei cammini minimi con la normalizzazione unaria `m[i,j] <= (m[i, not i] + m[not j, j]) / 2`.
   - Supporto ad assegnamenti affini esatti sia concordi che discordi: `y := x + c` e `y := -x + c`.
   - Filtro di vincoli unari, somme concordi (`x + y <= c`), somme negative (`-x - y <= c`) e differenze inverse (`-x + y <= c`).

---

## 📁 Struttura del Progetto

```text
.
├── bin/
│   ├── dune                  # Configurazione Dune per l'eseguibile CLI
│   └── main.ml               # Entry point CLI: riceve un file sorgente e valuta tutti i domini
├── lib/
│   ├── abstract_domains.ml   # Firme e implementazioni dei domini astratti
│   ├── dune                  # Configurazione Dune per la libreria (tesi_lib)
│   ├── interpeters.ml        # Funtori di analisi statica e moduli interprete istanziati
│   ├── parser.ml             # Lexer, Parser BNF a discesa ricorsiva e Pretty Printer
│   ├── shared_modules.ml     # Moduli condivisi: IntervalArith, DBMOperations, VariableRetrieval, SyntaxUtils, Fixpoint
│   └── syntax.ml             # Abstract Syntax Tree (bop, uop, exp, cond, cmd)
├── test/
│   ├── dune                  # Configurazione Dune per la suite di test
│   ├── oracles.ml            # Risultati attesi (oracoli) specifici per ciascun dominio
│   └── test_suite.ml         # Suite completa di 1121 test Alcotest per tutti i domini e il parser
├── test_program              # File di test d'esempio per il parser e la CLI
├── dune-project              # File radice di configurazione del progetto Dune
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

### Eseguire il Programma Principale (CLI)

Il programma `bin/main.exe` accetta come parametro il percorso di un file contenente il codice sorgente imperativo da analizzare. Esegue il parsing del codice e applica simultaneamente l'interpretazione astratta su tutti gli 8 domini, stampando lo stato invariante finale per ciascuno:

```bash
# Esempio con il file d'esempio incluso:
dune exec bin/main.exe -- test_program

# Oppure specificando un qualsiasi file sorgente:
dune exec bin/main.exe -- percorso/al/tuo_file.txt
```

> [!NOTE]
> Il file `test_program` contiene un esempio minimale con incremento e filtro relazionale:
> ```text
> x = 5;
> y = x++;
> y <= 5 ?
> ```
> L'analisi rileva che la condizione `y <= 5` risulta inconsistente (`Bottom`) nei domini relazionali e negli intervalli, poiché dopo l'incremento `y` assume valore 6.

### Eseguire i Test Unitari (Alcotest - 1121 Test)

Esegue la suite completa di **1121 test** automatizzati su tutti i domini astratti e sul parser:

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
- **Eseguire solo i test del Parser BNF**:
  ```bash
  dune exec ./test/test_suite.exe -- test "Parser BNF"
  ```
- **Eseguire un gruppo di test specifico per dominio**:
  ```bash
  dune exec ./test/test_suite.exe -- test "Zones: Cicli While"
  dune exec ./test/test_suite.exe -- test "Octagons: Sfide Specifiche Ottagonali"
  ```
- **Elencare tutti i 1121 test disponibili con i relativi indici**:
  ```bash
  dune exec ./test/test_suite.exe -- list
  ```

### REPL Interattivo (utop)

Per sperimentare in modo interattivo con il parser e i domini astratti:

1. Avviare `utop` caricando la libreria di progetto:
   ```bash
   opam exec -- dune utop lib
   ```

2. All'interno di `utop` (sfruttando direttamente il parser per scrivere programmi come stringhe):
   ```ocaml
   open Parser;;
   open Interpeters;;

   (* Parsing diretto del codice imperativo *)
   let p1 = parse_cmd "x = 0; while x < 10 do x = x + 2";;

   (* Analisi con il Dominio degli Intervalli *)
   let res_intervals = IntervalInterp.eval p1;;
   IntervalInterp.outputStatePrinter res_intervals;;

   (* Analisi con il Dominio delle Zone (DBM) *)
   let res_zone = ZoneInterp.eval p1;;
   ZoneInterp.print_result res_zone;;

   (* Analisi con il Dominio degli Ottagoni (DBM 2n x 2n) *)
   let res_oct = OctagonInterp.eval p1;;
   OctagonInterp.print_result res_oct;;
   ```

---

## 💻 Esempi di Utilizzo

### Esempio 1: Analisi da Codice Sorgente Testuale (Parser)

Grazie al modulo `Parser`, è possibile scrivere i programmi in sintassi testuale e analizzarli con poche righe di codice:

```ocaml
open Parser
open Interpeters

let source_code = "
  x = nondet(1, 5);
  y = -x + 10;
  z = x + 2;
  filter(y >= 5)
"

let () =
  let program = parse_cmd source_code in

  print_endline "=== Analisi con Intervalli ===";
  IntervalInterp.outputStatePrinter (IntervalInterp.eval program);

  print_endline "\n=== Analisi con Zone (DBM) ===";
  ZoneInterp.print_result (ZoneInterp.eval program);

  print_endline "\n=== Analisi con Ottagoni ===";
  OctagonInterp.print_result (OctagonInterp.eval program)
```

### Esempio 2: Costruzione Diretta dell'AST in OCaml

In alternativa al parser testuale, è possibile costruire programmaticamente l'AST tramite i costruttori esposti in `Syntax`:

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
