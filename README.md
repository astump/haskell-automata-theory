# HaskAut

This is a repository of Haskell implementations of various basic constructions in automata theory.
I am developing this as part of teaching a class on this topic at The University of Iowa.  My reference 
for the material is "Automata and Computability" by Dexter Kozen.

## To run

You may just do

```
cabal v1-configure
cabal v1-repl
```

from the top-level of this repo, and then try importing various
files, like this (from the repl that cabal v1-repl gives you):

```
import HaskAut.FiniteAutomata.DfaExamples
```
