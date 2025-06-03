

## `Main.hs` — Coordination, Benchmarking, and Cross-Validation

The `Main.hs` file serves as the central orchestration point of the application, but its role goes well beyond mere function invocation. This module demonstrates a deep understanding of Haskell's runtime behavior and the subtleties of lazy evaluation.

Each solver’s execution is preceded by an explicit garbage collection via `performGC`, ensuring that leftover thunks or memory from prior computations don’t interfere with benchmarking. This is crucial in a lazy language like Haskell, where computations can remain unevaluated until unexpectedly triggered. By resetting the GC, the author guarantees cleaner and more isolated performance metrics.

Timing is performed using `getCPUTime`, and memory usage is recorded via `getRTSStats`, leveraging GHC's runtime statistics to measure allocated bytes. These are advanced tools that indicate awareness of runtime profiling techniques covered in your slides. Moreover, the results are forced into normal form using `force` in combination with `evaluate`, ensuring that any lazily deferred computations are fully realized before measurement—critical for obtaining accurate timings in Haskell.

A particularly thoughtful addition is the cross-validation step, where the results from both the naïve backtracking solver and the optimized DPLL solver are compared. If their answers diverge, a warning is logged. This runtime cross-checking serves as an internal regression test, catching inconsistencies in logic or implementation—a sophisticated quality control strategy often overlooked in academic projects.

Finally, the results are written to a CSV file, enabling integration with external data analysis tools or pipelines. The use of structured output here aligns well with the principles of reproducible research and allows for automation in performance evaluation.

---

## `Expr.hs` — Boolean Expression Semantics and Structure

This module defines the core data structure for logical expressions via an algebraic data type (`Expr`) with constructors for variables, negation, conjunction, and disjunction. It’s an idiomatic and clean abstraction of propositional logic, but what stands out is the careful separation of syntax and semantics.

The `eval` function provides a pure interpretation mechanism that computes the truth value of an expression given an environment. This purity makes the module trivially testable using QuickCheck. For example, a property like `eval env (Not (Not e)) == eval env e` is easy to state and validate.

Moreover, the `vars` function extracts all the free variables from a formula. This function is key to domain generation in both solvers and is a natural fit for property-based testing: one could assert that every variable used during evaluation is contained within the list produced by `vars`.

The code here is also highly composable and lends itself to multiple reuse scenarios—whether in random expression generation, symbolic manipulation, or transformations to normal forms.

---

## `CNF.hs` — Structural Normalization and Clause Extraction

`CNF.hs` handles the syntactic transformation of boolean formulas into conjunctive normal form (CNF), a necessary preprocessing step for SAT solvers like DPLL. The design is functional and cleanly structured, breaking the conversion down into steps: flattening nested `Or`s, pushing down negations (via De Morgan), and eventually reducing to a list of clauses.

These transformations are deterministic and side-effect-free, which makes them excellent candidates for `doctest`-based regression testing, as encouraged in the slides. Annotating the functions with `-- >>>` inline tests would instantly transform this module into a verified transformation pipeline.

Moreover, by operating purely on immutable data, the module benefits from structural sharing and avoids unnecessary recomputation—making the CNF transformation relatively efficient despite its recursive nature.

This module bridges the gap between logical abstraction and algorithmic practicality. Its output format (list of clauses) serves as the input to the solver modules, and thus represents a central interface between logic and computation.

---

## `Backtracking.hs` — Exhaustive Search Without Heuristics

The `Backtracking.hs` module implements a brute-force solver that exhaustively enumerates all possible truth assignments and checks whether each one satisfies the formula. While computationally inefficient for large formulas, this approach is conceptually simple and useful as a baseline or correctness oracle.

The combinatorial generation of assignments via `mapM` over the list monad is a hallmark of elegant functional code. The solver cleanly separates assignment generation from satisfiability checking (`isSatisfied`), leading to better modularity and clearer testing boundaries.

One particularly interesting extension, hinted at in the slides, is the potential for parallelization using `parMap` from `Control.Parallel.Strategies`. Since each truth assignment can be evaluated independently, the problem is embarrassingly parallel—making this a textbook case for deterministic parallelism in Haskell.

This solver, although not efficient, is invaluable for testing the correctness of more advanced solvers such as DPLL. It provides a concrete ground truth against which the optimized version can be validated.

---

## `DPLL.hs` — Optimization Through Logic and Propagation

This module implements the DPLL (Davis–Putnam–Logemann–Loveland) algorithm, a significant optimization over naive backtracking. It incorporates intelligent heuristics like unit clause propagation and pure literal elimination to prune the search space early.

The recursive structure of the main `dpll` function is declarative and expressive. At each step, the algorithm applies simplifications (unit clauses, pure literals) before branching, thereby reducing unnecessary work. These techniques drastically reduce the number of recursive calls in practice and mirror the algorithmic enhancements discussed in the theoretical parts of the slides.

Pattern matching is used extensively to improve clarity and correctness. The use of `Maybe` to encode success/failure is idiomatic and aligns with functional principles of explicit error handling without exceptions.

From a testing perspective, one could easily define QuickCheck properties like: “if `dpll f == Just a`, then `a` satisfies all clauses of `f`”. These properties could be run against random CNF instances to validate the implementation.

An advanced enhancement would be to track the number of simplifications or decisions made and use those for profiling or visualization—perhaps with ThreadScope or custom metrics.
