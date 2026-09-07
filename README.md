# Abstract Interpreter & Static Analyzer in OCaml

![OCaml](https://img.shields.io/badge/OCaml-4.12%2B-orange.svg)
![Dune](https://img.shields.io/badge/Build%20System-Dune-blue.svg)
![Test](https://img.shields.io/badge/Testing-Alcotest-green.svg)

A static analysis tool and abstract interpreter written in OCaml for a custom imperative programming language. The interpreter is parameterized over arbitrary **Abstract Domains** (such as multiple Sign domains and an Interval domain) and computes the sound approximation of program executions using **Least Fixpoint (LFP)** iteration, **Widening**, **Narrowing**, and **Relational Environment Refinement**.

---

## 📋 Table of Contents

- [Overview](#overview)
- [Key Features](#key-features)
- [Abstract Domains](#abstract-domains)
- [Project Structure](#project-structure)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Building & Running](#building--running)
  - [Build the Project](#build-the-project)
  - [Execute the Main Program](#execute-the-main-program)
  - [Run Unit Tests (Alcotest)](#run-unit-tests-alcotest)
  - [Interactive REPL (utop)](#interactive-repl-utop)
- [Language Syntax & Example Program](#language-syntax--example-program)
- [License](#license)

---

## 🔍 Overview

This project implements a parametric abstract interpreter (`AbsInterp`) for analyzing imperative programs statically without executing them dynamically. 

The analyzer evaluates programs composed of:
- **Expressions**: Constants, variables, arithmetic operations (`+`, `-`, `*`, `/`), unary negation, and non-deterministic choice (`Random(min, max)`).
- **Conditions**: Comparisons (`=`, `>`, `<`, `>=`, `<=`, `<>`), boolean logic (`Not`, `And`, `Or`).
- **Commands**: Variable assignment (`Assign`), command sequences (`Sequence`), conditionals (`If`), guard filters (`Filter`), no-op (`Skip`), and loops (`While`).

It determines safety properties, variable value bounds, or signs at program termination using Kleene fixed-point iteration over lattice structures.

---

## ✨ Key Features

- **Parametric Architecture**: Functor-based design (`module AbsInterp (D : DOMAIN)`) allowing any abstract domain conforming to the `DOMAIN` signature to be plugged into the interpreter.
- **Multiple Abstract Domains**: Support for 5 variants of Sign Domains and an Interval Domain with infinite bounds ($-\infty, +\infty$).
- **Kleene Fixpoint & Widening/Narrowing**: Guarantees termination for loop analysis (`While`) using widening (`widen`) to accelerate convergence and narrowing (`narrow`) to recover precision.
- **Relational Condition Filtering**: Refines environment variable states (`refine_vars`) during condition evaluation (`If` / `Filter`) using Greatest Lower Bound (`glb`) operations.
- **Comprehensive Test Suite**: Integrated test engine powered by [Alcotest](https://github.com/mirage/alcotest) verifying expression evaluations, branching, and while loops across domains.

---

## 📐 Abstract Domains

The interpreter includes several abstract domain implementations defined in `lib/abstract_domains.ml`:

1. **`Signs`**: Complete 8-element sign domain:
   $$\{\top, >0, \ge 0, =0, \le 0, <0, \neq 0, \bot\}$$
2. **`SimpleSigns`**: 5-element domain containing $\{\top, \ge 0, 0, \le 0, \bot\}$.
3. **`SimplifiedSigns`**: 5-element domain containing $\{\top, >0, 0, <0, \bot\}$.
4. **`ReducedSigns`**: Minimal 4-element domain containing $\{\top, >0, <0, \bot\}$.
5. **`StrangeSigns`**: 5-element experimental domain $\{\top, \ge 0, 0, <0, \bot\}$.
6. **`Intervals`**: Interval domain $[l, u]$ where $l, u \in \{-\infty, \mathbb{Z}, +\infty\}$, supporting interval arithmetic, bounds narrowing, and infinite limits.

---

## 📁 Project Structure

```text
.
├── bin/
│   ├── dune             # Dune build configuration for executable
│   └── main.ml          # Entry point for program execution and testing sample ASTs
├── lib/
│   ├── abstract_domains.ml  # Definition of DOMAIN signature and domain modules
│   ├── dune                 # Dune build configuration for the library
│   ├── interpeters.ml       # AbsInterp functor & static analysis engine
│   └── syntax.ml            # AST definition (bop, uop, exp, cond, cmd)
├── test/
│   ├── dune             # Dune configuration for test suite
│   ├── oracles.ml       # Expected outputs for unit tests
│   └── test_suite.ml    # Alcotest test cases (expressions, control flow, loops)
├── dune-project         # Dune project definition
└── README.md            # Project documentation
```

---

## 🛠️ Prerequisites

To build and run this project, you need:

- **OCaml** ($\ge 4.12$)
- **Opam** (OCaml Package Manager)
- **Dune** ($\ge 3.0$)
- **Alcotest** (for running unit tests)
- **Utop** (optional, for interactive REPL exploration)

---

## 📥 Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/TaleWind28/Tesi.git
   cd Tesi
   ```

2. **Initialize or update Opam environment**:
   ```bash
   opam switch create . 4.14.0  # Or use an existing switch
   eval $(opam env)
   ```

3. **Install dependencies**:
   ```bash
   opam install dune alcotest utop
   ```

---

## 🚀 Building & Running

### Build the Project

Build all libraries, executables, and tests:
```bash
opam exec -- dune build
```

### Execute the Main Program

Run the main analysis script (`bin/main.ml`):
```bash
opam exec -- dune exec bin/main.exe
```

### Run Unit Tests (Alcotest)

Run the automated test suite across all implemented abstract domains:
```bash
opam exec -- dune runtest
```

#### Useful Testing Flags:

- **Force re-run (ignoring build cache)**:
  ```bash
  opam exec -- dune runtest -f
  ```
- **Watch mode (re-runs tests automatically when code changes)**:
  ```bash
  opam exec -- dune runtest -f -w
  ```
- **Run a specific test by name**:
  ```bash
  opam exec ./test/test_suite.exe -- test "Signs: Somma"
  ```
- **List all available tests**:
  ```bash
  opam exec ./test/test_suite.exe -- list
  ```

### Interactive REPL (utop)

To experiment with abstract domains and program execution interactively:

1. Launch `utop` preloaded with project libraries:
   ```bash
   opam exec -- dune utop lib
   ```

2. Inside `utop`, load the modules or main file:
   ```ocaml
   open Syntax;;
   open Abstract_domains;;
   open Interpeters;;

   (* Or load main.ml directly *)
   #use "bin/main.ml";;
   ```

3. Evaluate abstract execution manually:
   ```ocaml
   let prog = Assign ("x", Const 5);;
   IntervalInterp.eval prog;;
   ```

---

## 💻 Language Syntax & Example Program

Programs are constructed using OCaml AST constructs defined in `lib/syntax.ml`:

```ocaml
open Syntax
open Interpeters

(* Program: 
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
  print_string "Resulting Abstract Environment (Intervals):\n";
  IntervalInterp.outputStatePrinter result
```

---

## 📜 License

Distributed under the MIT License. See `LICENSE` for more details (if applicable).
