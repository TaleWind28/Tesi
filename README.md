# Interprete Astratto & Analizzatore Statico in OCaml

![OCaml](https://img.shields.io/badge/OCaml-4.12%2B-orange.svg)
![Dune](https://img.shields.io/badge/Build%20System-Dune-blue.svg)
![Test](https://img.shields.io/badge/Testing-Alcotest-green.svg)

Un tool di analisi statica ed interpretazione astratta scritto in OCaml per un linguaggio di programmazione imperativo. L'interprete è parametrizzato su **Domini Astratti** generici, includendo sia domini non-relazionali (5 varianti del dominio dei Segni e il dominio degli Intervalli) sia un dominio debolmente relazionale (il dominio delle **Zone** basato su DBM). Calcola l'approssimazione corretta dell'esecuzione dei programmi mediante punto fisso, tecniche di **Widening**, **Narrowing** e **Raffinamento dei Vincoli**.

---

## 📋 Indice dei Contenuti

- [Panoramica](#-panoramica)
- [Caratteristiche Principali](#-caratteristiche-principali)
- [Domini Astratti](#-domini-astratti)
  - [Domini Non-Relazionali](#domini-non-relazionali)
  - [Dominio Debolmente Relazionale (Zone)](#dominio-debolmente-relazionale-zone)
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

Determina le proprietà di sicurezza, i limiti numerici delle variabili, i segni o le relazioni di differenza nello stato finale di terminazione del programma.

---

## ✨ Caratteristiche Principali

- **Architettura Parametrica a Funtori**:
  - `NonRelationalAbsInterp (D : NonRelationalDomain)`: motore per domini non-relazionali che mappano identificatori a valori astratti indipendenti (`(string, D.t) Hashtbl.t`).
  - `WeakRelationalAbsInterp (D : WeakRelationalDomain)`: motore relazionale per vincoli tra variabili.
- **Molteplici Domini Astratti**:
  - 5 varianti del Dominio dei Segni con diversi livelli di granularità.
  - Dominio degli Intervalli con estremi estesi (`-oo`, `+oo`).
  - Dominio delle Zone basato su matrici di vincoli di differenza (DBM).
- **Calcolo del Punto Fisso e Widening/Narrowing**:
  - Accelerazione della convergenza nei cicli `While` tramite operatore di **Widening** (`widen`).
  - Recupero della precisione persa post-convergenza tramite operatore di **Narrowing** (`narrow`).
- **Raffinamento delle Condizioni**:
  - Filtri e diramazioni (`If`, `Filter`) raffinano lo stato delle variabili tramite intersezione/least upper bound (`lub`) e vincoli di disuguaglianza.
- **Suite di Test Completa**:
  - Oltre 500 test automatizzati basati su [Alcotest](https://github.com/mirage/alcotest) per verificare la correttezza di ogni operatore e costrutto sintattico su ciascun dominio implementato.

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
6. **`Intervals`**: Dominio degli intervalli `[lo, hi]` con `lo, hi` in `{-oo, Int n, +oo}`, con aritmetica per intervalli, estensione non deterministica e raffinamento dei limiti.

### Dominio Debolmente Relazionale (Zone)

7. **`Zones`**: Dominio relazionale basato su **Difference Bound Matrices (DBM)** per tracciare vincoli del tipo:
   - Differenze tra coppie di variabili: `x - y <= c`
   - Limiti individuali rispetto alla variabile zero `v0`: `x <= c` e `x >= c`
   - Chiusura canonica dei cammini minimi mediante algoritmo di **Floyd-Warshall**
   - Rilevamento di cicli di peso negativo per identificare stati irraggiungibili (`Bottom`)
   - Operazioni di riassegnamento (`assign_const`, `assign_var_offset`), traslazione (`shift_var`), widening e narrowing relazionali.

---

## 📁 Struttura del Progetto

```text
.
├── bin/
│   ├── dune                  # Configurazione Dune per l'eseguibile main
│   └── main.ml               # Entry point di esempio con analisi su tutti i domini
├── lib/
│   ├── abstract_domains.ml   # Firme (NonRelationalDomain, WeakRelationalDomain) e moduli dei domini
│   ├── dune                  # Configurazione Dune per la libreria (tesi_lib)
│   ├── interpeters.ml        # Funtori di analisi statica (NonRelationalAbsInterp, WeakRelationalAbsInterp)
│   ├── shared_arithmetic.ml  # Operazioni aritmetiche su bound estesi (+/- inf) e intervalli
│   └── syntax.ml             # Abstract Syntax Tree (bop, uop, exp, cond, cmd)
├── test/
│   ├── dune                  # Configurazione Dune per la suite di test
│   ├── oracles.ml            # Risultati attesi (oracoli) specifici per ogni dominio
│   └── test_suite.ml         # Suite di test Alcotest per tutti i domini
├── dune-project              # File di progetto Dune
└── README.md                 # Documentazione del progetto
```

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

Esegue la suite completa di test automatizzati su tutti i domini:
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
   ```

---

## 💻 Sintassi del Linguaggio ed Esempi

I programmi vengono costruiti mediante i costruttori AST definiti in `lib/syntax.ml`:

```ocaml
open Syntax
open Interpeters

(* Programma: 
   x = -5;
   while (x < 0) {
     x = x + 1;
   }
*)
let program =
  Sequence (
    Assign ("x", Const (-5)),
    While (
      Comparison (Var "x", Smaller, Const 0),
      Assign ("x", BinaryOperation (Var "x", Add, Const 1))
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
  ZoneInterp.print_result res_zones
```

---

## 📜 Licenza

Distribuito sotto licenza MIT. Per maggiori informazioni vedere il file `LICENSE` (se presente).
