# Interprete Astratto & Analizzatore Statico in OCaml

![OCaml](https://img.shields.io/badge/OCaml-4.12%2B-orange.svg)
![Dune](https://img.shields.io/badge/Build%20System-Dune-blue.svg)
![Test](https://img.shields.io/badge/Testing-Alcotest-green.svg)

Un tool di analisi statica ed interpretazione astratta scritto in OCaml per un linguaggio di programmazione imperativo personalizzato. L'interprete è parametrizzato su **Domini Astratti** generici (tra cui diverse varianti del dominio dei Segni e il dominio degli Intervalli) e calcola l'approssimazione corretta dell'esecuzione dei programmi mediante l'iterazione del **Minimo Punto Fisso (Least Fixpoint - LFP)**, tecniche di **Widening**, **Narrowing** e **Raffinamento Relazionale dell'Ambiente**.

---

## 📋 Indice dei Contenuti

- [Panoramica](#-panoramica)
- [Caratteristiche Principali](#-caratteristiche-principali)
- [Domini Astratti](#-domini-astratti)
- [Struttura del Progetto](#-struttura-del-progetto)
- [Prerequisiti](#-prerequisiti)
- [Installazione](#-installazione)
- [Compilazione ed Esecuzione](#-compilazione-ed-esecuzione)
  - [Compilare il Progetto](#compilare-il-progetto)
  - [Eseguire il Programma Principale](#eseguire-il-programma-principale)
  - [Eseguire i Test Unitari (Alcotest)](#eseguire-i-test-unitari-alcotest)
  - [REPL Interattivo (utop)](#repl-interattivo-utop)
- [Sintassi del Linguaggio ed Esempio](#-sintassi-del-linguaggio-ed-esempio)
- [Licenza](#-licenza)

---

## 🔍 Panoramica

Questo progetto implementa un interprete astratto parametrico (`AbsInterp`) per analizzare staticamente i programmi imperativi senza eseguirli dinamicamente.

L'analizzatore valuta programmi composti da:
- **Espressioni**: Costanti, variabili, operazioni aritmetiche (`+`, `-`, `*`, `/`), negazione unaria e scelte non deterministiche (`Random(min, max)`).
- **Condizioni**: Comparazioni (`=`, `>`, `<`, `>=`, `<=`, `<>`), logica booleana (`Not`, `And`, `Or`).
- **Comandi**: Assegnamento a variabili (`Assign`), sequenze di comandi (`Sequence`), istruzioni condizionali (`If`), filtri di guardia (`Filter`), no-op (`Skip`) e cicli (`While`).

Determina le proprietà di sicurezza, i limiti dei valori delle variabili e i segni nello stato finale di terminazione del programma utilizzando l'iterazione di Kleene su strutture a reticolo (lattice).

---

## ✨ Caratteristiche Principali

- **Architettura Parametrica**: Progettazione basata su funtori OCaml (`module AbsInterp (D : DOMAIN)`) che permette di collegare all'interprete qualsiasi dominio astratto conforme alla firma `DOMAIN`.
- **Molteplici Domini Astratti**: Supporto per 5 varianti del Dominio dei Segni e per un Dominio degli Intervalli con estremi infiniti ($-\infty, +\infty$).
- **Calcolo del Punto Fisso e Widening/Narrowing**: Garantisce la terminazione dell'analisi dei cicli (`While`) tramite widening (`widen`) per accelerare la convergenza e narrowing (`narrow`) per recuperare la precisione persa.
- **Raffinamento Relazionale nelle Condizioni**: Raffina lo stato delle variabili nell'ambiente (`refine_vars`) durante la valutazione delle condizioni (`If` / `Filter`) attraverso operazioni di Greatest Lower Bound (`glb`).
- **Suite di Test Completa**: Engine di testing integrato basato su [Alcotest](https://github.com/mirage/alcotest) per verificare la correttezza di espressioni, diramazioni condizionali e cicli while su tutti i domini implementati.

---

## 📐 Domini Astratti

L'interprete include diverse implementazioni di domini astratti definite in `lib/abstract_domains.ml`:

1. **`Signs`**: Dominio dei segni completo a 8 elementi:
   $$\{\top, >0, \ge 0, =0, \le 0, <0, \neq 0, \bot\}$$
2. **`SimpleSigns`**: Dominio a 5 elementi $\{\top, \ge 0, 0, \le 0, \bot\}$.
3. **`SimplifiedSigns`**: Dominio a 5 elementi $\{\top, >0, 0, <0, \bot\}$.
4. **`ReducedSigns`**: Dominio essenziale a 4 elementi $\{\top, >0, <0, \bot\}$.
5. **`StrangeSigns`**: Dominio sperimentale a 5 elementi $\{\top, \ge 0, 0, <0, \bot\}$.
6. **`Intervals`**: Dominio degli intervalli $[l, u]$ dove $l, u \in \{-\infty, \mathbb{Z}, +\infty\}$, con supporto ad aritmetica degli intervalli, raffinamento dei limiti e confini infiniti.

---

## 📁 Struttura del Progetto

```text
.
├── bin/
│   ├── dune             # Configurazione Dune per l'eseguibile
│   └── main.ml          # Entry point per l'esecuzione e il test di AST di esempio
├── lib/
│   ├── abstract_domains.ml  # Definizione della signature DOMAIN e dei moduli dei domini
│   ├── dune                 # Configurazione Dune per la libreria
│   ├── interpeters.ml       # Funtore AbsInterp & motore di analisi statica
│   └── syntax.ml            # Definizione dell'AST (bop, uop, exp, cond, cmd)
├── test/
│   ├── dune             # Configurazione Dune per la suite di test
│   ├── oracles.ml       # Risultati attesi (oracoli) per i test unitari
│   └── test_suite.ml    # Test case Alcotest (espressioni, flusso di controllo, cicli)
├── dune-project         # Definizione del progetto Dune
└── README.md            # Documentazione del progetto (in italiano)
```

---

## 🛠️ Prerequisiti

Per compilare ed eseguire il progetto sono necessari:

- **OCaml** ($\ge 4.12$)
- **Opam** (OCaml Package Manager)
- **Dune** ($\ge 3.0$)
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

Compila tutte le librerie, gli eseguibili e i test:
```bash
opam exec -- dune build
```

### Eseguire il Programma Principale

Esegue lo script di analisi principale (`bin/main.ml`):
```bash
opam exec -- dune exec bin/main.exe
```

### Eseguire i Test Unitari (Alcotest)

Esegue la suite di test automatizzati su tutti i domini astratti implementati:
```bash
opam exec -- dune runtest
```

#### Flag Utili per il Testing:

- **Forzare la riesecuzione (ignorando la cache di build)**:
  ```bash
  opam exec -- dune runtest -f
  ```
- **Modalità Watch (riesegue i test automaticamente ad ogni modifica del codice)**:
  ```bash
  opam exec -- dune runtest -f -w
  ```
- **Eseguire un test specifico per nome**:
  ```bash
  opam exec ./test/test_suite.exe -- test "Signs: Somma"
  ```
- **Elencare tutti i test disponibili**:
  ```bash
  opam exec ./test/test_suite.exe -- list
  ```

### REPL Interattivo (utop)

Per sperimentare in modo interattivo con i domini astratti e l'esecuzione dei programmi:

1. Avviare `utop` precaricando le librerie del progetto:
   ```bash
   opam exec -- dune utop lib
   ```

2. All'interno di `utop`, caricare i moduli o il file principale:
   ```ocaml
   open Syntax;;
   open Abstract_domains;;
   open Interpeters;;

   (* Oppure caricare direttamente main.ml *)
   #use "bin/main.ml";;
   ```

3. Valutare manualmente un programma astratto:
   ```ocaml
   let prog = Assign ("x", Const 5);;
   IntervalInterp.eval prog;;
   ```

---

## 💻 Sintassi del Linguaggio ed Esempio

I programmi vengono costruiti utilizzando i costruttori AST definiti in `lib/syntax.ml`:

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
  let result = IntervalInterp.eval program in
  print_string "Stato Astratto Risultante (Intervalli):\n";
  IntervalInterp.outputStatePrinter result
```

---

## 📜 Licenza

Distribuito sotto licenza MIT. Per maggiori informazioni vedere il file `LICENSE` (se presente).
